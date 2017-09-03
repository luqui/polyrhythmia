var PolyrhythmiaModule = function(jQuery) {

$$ = {};

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


$$.Stack = function() {
    this.symbols = [];
};

$$.Stack.prototype.draw = function(ctx, grid) {
    var accum = 0;
    for (var i = 0; i < this.symbols.length; i++) {
        this.symbols[i].draw(ctx, grid, accum);
        accum += this.symbols[i].size();
    }
};

$$.Stack.prototype.push = function(symbol) {
    this.symbols.push(symbol);
};


$$.Rhythm = function(subdiv, patternlen) {
    this.subdiv = subdiv;
    this.patternlen = patternlen;
};

$$.Rhythm.prototype.size = function() {
    return this.patternlen * this.subdiv.asNumber();
};

$$.Rhythm.prototype.draw = function(ctx, grid, xbase) {
    ctx.fillStyle = '#ff0000';
    var x0 = grid.locateX(xbase);
    var x1 = grid.locateX(xbase + this.size());
    var y0 = grid.locateY(0);
    var y1 = grid.locateY(1);
    ctx.fillRect(x0, y0, x1 - x0, y1 - y0);
};


$$.Sequence = function(numtracks, bounds) {
    this.tracks = [];
    this.bounds = bounds;
    for (var i = 0; i < numtracks; i++) {
        this.tracks.push(new $$.Stack());
    }
};

$$.Sequence.prototype.draw = function(ctx) {
    for (var i = 0; i < this.tracks.length; i++) {
        let dy = (this.bounds.y1 - this.bounds.y0)/this.tracks.length;
        this.tracks[i].draw(
            ctx, 
            new $$.Grid(this.bounds.x0, 
                        this.bounds.y0 + i*dy,
                        this.bounds.x0 + (this.bounds.x1 - this.bounds.x0) / 100,
                        this.bounds.y0 + (i+1)*dy));

    }
};

$$.Sequence.prototype.rowYBounds = function(row) {
    let dy = (this.bounds.y1 - this.bounds.y0) / this.tracks.length;
    let r = [ this.bounds.y0 + row*dy, this.bounds.y0 + (row+1)*dy ];
    return r;
};


$$.Game = function(numtracks, bounds) {
    this.bounds = bounds;
    var slice = this.bounds.sliceX(0.75);
    this.sequence = new $$.Sequence(numtracks, slice[0]);
    

    this.piece = new $$.Rhythm(new $$.Rational(1,1), 10);
    this.pieceColBounds = slice[1];
    this.pieceRow = 0;
};

$$.Game.prototype.draw = function(ctx) {
    this.sequence.draw(ctx);
    if (this.piece) {
        let ybounds = this.sequence.rowYBounds(this.pieceRow);
        this.piece.draw(ctx, new $$.Grid(this.pieceColBounds.x0, ybounds[0], this.pieceColBounds.x0 + 10, ybounds[1]), 0);
    }
};

$$.Game.prototype.insert = function() {
    if (this.piece) {
        this.sequence.tracks[this.pieceRow].push(this.piece);
        this.piece = new $$.Rhythm(new $$.Rational(1,1), 10);
    }
};

$$.Game.prototype.prevTrack = function() {
    if (this.pieceRow > 0) { this.pieceRow--; }
};

$$.Game.prototype.nextTrack = function() {
    if (this.pieceRow < this.sequence.tracks.length-1) { this.pieceRow++; }
};

return $$;

};

