var PolyrhythmiaModule = function(jQuery) {

var $$ = {};

var gcd = function(a,b) {
    if (b > a) {var temp = a; a = b; b = temp;}
    while (true) {
        if (b == 0) return a;
        a %= b;
        if (a == 0) return b;
        b %= a;
    }
}


$$.Rational = function(numerator, denominator) {
    var div = gcd(numerator, denominator);
    this.numerator = numerator / div;
    this.denominator = denominator / div;
};

$$.Rational.prototype.asNumber = function() {
    return this.numerator / this.denominator;
};


// Like a Grid, but is actually a bounding box (rather than a y box and an x scale)
$$.Bounds = function(x0, y0, x1, y1) {
    this.x0 = x0;
    this.y0 = y0;
    this.x1 = x1;
    this.y1 = y1;
};

$$.Bounds.prototype.sliceX = function(percent) {
    return [
        new $$.Bounds(this.x0, this.y0, this.x0 + (this.x1-this.x0)*percent, this.y1),
        new $$.Bounds(this.x0 + (this.x1-this.x0)*percent, this.y0, this.x1, this.y1)
    ];
};


// Only axis-aligned transformations are allowed, because we're drawing
// rectangles.
$$.Grid = function(x0, y0, x1, y1) {
    this.x0 = x0;
    this.y0 = y0;
    this.x1 = x1;
    this.y1 = y1;
};

$$.Grid.prototype.locateX = function(x) {
    return((this.x1-this.x0)*x + this.x0);
};

$$.Grid.prototype.locateY = function(y) {
    return((this.y1-this.y0)*y + this.y0);
};

$$.Grid.prototype.restrictY = function(start, height) {
    return new $$.Grid(this.x0, this.y0 + (this.y1-this.y0)*start, this.x1, this.y0 + (this.y1-this.y0)*(start + height));
};


$$.Rhythm = function(subdiv, ch, note, pattern) {
    this.subdiv = subdiv;
    this.channel = ch;
    this.note = note;
    this.pattern = pattern;
};

$$.Rhythm.prototype.size = function() {
    return this.pattern.length * this.subdiv.asNumber();
};

$$.Rhythm.prototype.draw = function(ctx, grid, xbase) {
    ctx.fillStyle = '#ff0000';
    var x0 = grid.locateX(xbase);
    var x1 = grid.locateX(xbase + this.size());
    var y0 = grid.locateY(0);
    var y1 = grid.locateY(1);
    ctx.fillRect(x0, y0, x1 - x0, y1 - y0);
};

$$.Rhythm.prototype.play = function(midi, t0, dt) {
    var div = this.subdiv.asNumber();
    var ix0 = Math.floor(t0/div)+1;
    var ix1 = Math.floor((t0+dt)/div);
    while (ix0 <= ix1 && ix0 < this.pattern.length) {
        console.log("note on ", ix0);
        midi.noteOn(this.channel, this.note, this.pattern[ix0]);
        ix0++;
    }
};



$$.Stack = function() {
    this.symbols = [];
};

$$.Stack.prototype.draw = function(ctx, grid) {
    var accum = 0;
    for (var i = 0; i < this.symbols.length; i++) {
        this.symbols[i].symbol.draw(ctx, grid, accum);
        accum = this.symbols[i].endtime;
    }
};

$$.Stack.prototype.push = function(symbol) {
    if (this.symbols.length == 0) {
        var endtime = 0;
    }
    else {
        var endtime = this.symbols[this.symbols.length-1].endtime;
    }
    this.symbols.push({ symbol: symbol, endtime: endtime + symbol.size() });
};

$$.Stack.prototype.play = function(midi, t0, dt) {
    var starttime = 0;
    var i;
    for (i = 0; i < this.symbols.length; i++) {
        if (t0 + dt >= starttime && t0 < this.symbols[i].endtime) {
            this.symbols[i].symbol.play(midi, t0 - starttime, dt);
        }
        starttime = this.symbols[i].endtime;
        if (starttime > t0+dt) { break; }
    }
};


$$.Sequence = function(numtracks, bounds) {
    this.tracks = [];
    this.bounds = bounds;
    this.grid = new $$.Grid(bounds.x0, bounds.y0, 10, bounds.y1);
    for (var i = 0; i < numtracks; i++) {
        this.tracks.push(new $$.Stack());
    }
};

$$.Sequence.prototype.draw = function(ctx) {
    for (var i = 0; i < this.tracks.length; i++) {
        let dy = (this.bounds.y1 - this.bounds.y0)/this.tracks.length;
        this.tracks[i].draw(
            ctx, 
            this.grid.restrictY(i / this.tracks.length, 1 / this.tracks.length));
    }
};

$$.Sequence.prototype.rowYBounds = function(row) {
    let dy = (this.bounds.y1 - this.bounds.y0) / this.tracks.length;
    let r = [ this.bounds.y0 + row*dy, this.bounds.y0 + (row+1)*dy ];
    return r;
};

$$.Sequence.prototype.play = function(midi, t0, dt) {
    for (var i = 0; i < this.tracks.length; i++) {
        this.tracks[i].play(midi, t0, dt);
    }
};


$$.Game = function(numtracks, bounds) {
    this.bounds = bounds;
    var slice = this.bounds.sliceX(0.75);
    this.sequence = new $$.Sequence(numtracks, slice[0]);
    
    this.piece = this.genRhythm();
    this.pieceColBounds = slice[1];
    this.pieceRow = 0;

    this.time = -1e-3;  // so we play first note
};

$$.Game.prototype.draw = function(ctx) {
    this.sequence.draw(ctx);
    if (this.piece) {
        let ybounds = this.sequence.rowYBounds(this.pieceRow);
        this.piece.draw(ctx, new $$.Grid(this.pieceColBounds.x0, ybounds[0], this.pieceColBounds.x0 + 10, ybounds[1]), 0);
    }

    ctx.strokeStyle = '#00ff00';
    ctx.beginPath();
    ctx.moveTo(this.sequence.grid.locateX(this.time), this.sequence.grid.locateY(0));
    ctx.lineTo(this.sequence.grid.locateX(this.time), this.sequence.grid.locateY(1));
    ctx.stroke();
};

$$.Game.prototype.genRhythm = function() {
    return new $$.Rhythm(new $$.Rational(1,1), 1, 64, [96,0,96,96,0,96,0,0]);
};

$$.Game.prototype.insert = function() {
    if (this.piece) {
        this.sequence.tracks[this.pieceRow].push(this.piece);
        this.piece = this.genRhythm();
    }
};

$$.Game.prototype.prevTrack = function() {
    if (this.pieceRow > 0) { this.pieceRow--; }
};

$$.Game.prototype.nextTrack = function() {
    if (this.pieceRow < this.sequence.tracks.length-1) { this.pieceRow++; }
};

$$.Game.prototype.advance = function(midi, dt) {
    this.sequence.play(midi, this.time, dt);
    this.time += dt;
};

return $$;

};

