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

$$.Rational.prototype.scale = function(n) {
    return new $$.Rational(n*this.numerator, this.denominator);
};

$$.Rational.prototype.show = function() {
    return "" + this.numerator + "/" + this.denominator;
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

    var subdiv = this.subdiv.asNumber();
    ctx.fillStyle = '#000000';
    for (var i = 0; i < this.pattern.length; i++) {
        let v = this.pattern[i];
        if (v != 0) {
            let x = grid.locateX(xbase+subdiv*i);
            let y = grid.locateY(0.5);
            ctx.globalAlpha = v/127;
            ctx.fillRect(x-1,y-1,2,2);
        }
    }
    ctx.globalAlpha = 1;

    ctx.fillStyle = '#000000';
    ctx.font = '18px serif';
    ctx.fillText(this.subdiv.scale(this.pattern.length).show(), x0, y0+24);
};

$$.Rhythm.prototype.play = function(midi, t0, dt) {
    var div = this.subdiv.asNumber();
    var ix0 = Math.floor(t0/div)+1;
    var ix1 = Math.floor((t0+dt)/div);
    while (ix0 <= ix1 && ix0 < this.pattern.length) {
        midi.noteOn(this.channel, this.note, this.pattern[ix0]);
        midi.noteOn(this.channel, this.note, 0);
        ix0++;
    }
};


$$.Palette = function() {
    this._rhythms = [];
    this._selected = null;
};

$$.Palette.prototype.add = function(rhythm, count) {
    this._rhythms.push({ rhythm: rhythm, count: count });
    if (this._selected === null) {
        this._selected = 0;
    }
};

$$.Palette.prototype.draw = function(ctx, grid) {
    var x = 0;
    for (var i = 0; i < this._rhythms.length; i++) {
        let size = this._rhythms[i].rhythm.size() 
        this._rhythms[i].rhythm.draw(ctx, grid, x);
        if (i == this._selected) {
            ctx.strokeStyle = '#0000ff';
            ctx.lineWidth = 3;
            ctx.strokeRect(grid.locateX(x), grid.locateY(0), grid.locateX(x+size)-grid.locateX(x), grid.locateY(1)-grid.locateY(0));
        }
        ctx.fillStyle = '#000000';
        ctx.font = '18px serif';
        ctx.fillText(this._rhythms[i].count, grid.locateX(x)+12, grid.locateY(1)-12);
        x += size + 1;
    }
};

$$.Palette.prototype.nextItem = function() {
    if (this._rhythms && this._selected < this._rhythms.length-1) {
        this._selected++;
    }
};

$$.Palette.prototype.prevItem = function() {
    if (this._rhythms && this._selected > 0) {
        this._selected--;
    }
};

$$.Palette.prototype.takeCurrent = function() {
    if (this._rhythms && this._rhythms[this._selected].count > 0) {
        this._rhythms[this._selected].count--;
        return this._rhythms[this._selected].rhythm;
    }
    else {
        return null;
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
    this._tracks = [];
    this._bounds = bounds;
    this._grid = new $$.Grid(bounds.x0, bounds.y0, 10, bounds.y1);
    for (var i = 0; i < numtracks; i++) {
        this._tracks.push(new $$.Stack());
    }
};

$$.Sequence.prototype.draw = function(ctx) {
    for (var i = 0; i < this._tracks.length; i++) {
        let dy = (this._bounds.y1 - this._bounds.y0)/this._tracks.length;
        this._tracks[i].draw(
            ctx, 
            this._grid.restrictY(i / this._tracks.length, 1 / this._tracks.length));
    }
};

$$.Sequence.prototype.rowYBounds = function(row) {
    let dy = (this._bounds.y1 - this._bounds.y0) / this._tracks.length;
    let r = [ this._bounds.y0 + row*dy, this._bounds.y0 + (row+1)*dy ];
    return r;
};

$$.Sequence.prototype.play = function(midi, t0, dt) {
    for (var i = 0; i < this._tracks.length; i++) {
        this._tracks[i].play(midi, t0, dt);
    }
};

$$.Sequence.prototype.grid = function() {
    return this._grid;
};

$$.Sequence.prototype.insert = function(track, rhythm) {
    this._tracks[track].push(rhythm);
};

$$.Sequence.prototype.numTracks = function() {
    return this._tracks.length;
};


$$.Game = function(numtracks, bounds) {
    this.bounds = bounds;
    var slice = this.bounds.sliceX(0.75);
    this.sequence = new $$.Sequence(numtracks, slice[0]);
    this.time = -1e-3;  // so we play first note

    this.palettes = [];
    for (var i = 0; i < numtracks; i++) {
        let pal = new $$.Palette();
        pal.add(this.genRhythm(), 10);
        pal.add(this.genRhythm(), 10);
        this.palettes.push(pal);
    }

    this.paletteBounds = slice[1];
    this.activeRow = 0;
};

$$.Game.prototype.draw = function(ctx) {
    this.sequence.draw(ctx);

    let grid = this.sequence.grid();
    ctx.strokeStyle = '#00ff00';
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(grid.locateX(this.time), grid.locateY(0));
    ctx.lineTo(grid.locateX(this.time), grid.locateY(1));
    ctx.stroke();

    for (var i = 0; i < this.palettes.length; i++) {
        let ybounds = this.sequence.rowYBounds(i);
        this.palettes[i].draw(ctx, 
            new $$.Grid(this.paletteBounds.x0, ybounds[0], this.paletteBounds.x0+10, ybounds[1]));
        if (this.activeRow == i) {
            ctx.strokeStyle = '#00ff00';
            ctx.lineWidth = 5;
            ctx.strokeRect(this.paletteBounds.x0, ybounds[0], this.paletteBounds.x1-this.paletteBounds.x0, ybounds[1]-ybounds[0]);
        }
    }
};

$$.Game.prototype.genRhythm = function() {
    var channel = Math.floor(Math.random()*4);
    var note = Math.floor(Math.random()*(83-36))+36;

    var len = [4,5,6,8,10,12][Math.floor(Math.random()*6)];
    var vels = [];
    for (var i = 0; i < len; i++) {
        if (Math.random() < 0.5) {
            vels.push(0);
        }
        else {
            vels.push(Math.floor(Math.random()*(127-32)+32));
        }
    }

    return new $$.Rhythm(new $$.Rational(1,2), channel, note, vels);
};

$$.Game.prototype.insert = function() {
    var rhythm = this.palettes[this.activeRow].takeCurrent();
    if (rhythm) {
        this.sequence.insert(this.activeRow, rhythm);
    }
};

$$.Game.prototype.prevTrack = function() {
    if (this.activeRow > 0) { this.activeRow--; }
};

$$.Game.prototype.nextTrack = function() {
    if (this.activeRow < this.palettes.length-1) { this.activeRow++; }
};

$$.Game.prototype.prevItem = function() {
    this.palettes[this.activeRow].prevItem();
};

$$.Game.prototype.nextItem = function() {
    this.palettes[this.activeRow].nextItem();
};

$$.Game.prototype.advance = function(midi, dt) {
    this.sequence.play(midi, this.time, dt);
    this.time += dt;
};

return $$;

};

