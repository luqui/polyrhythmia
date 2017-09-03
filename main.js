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
    console.log(x0, y0, x1, y1);
    ctx.fillRect(x0, y0, x1 - x0, y1 - y0);
};


$$.Sequence = function(numtracks) {
    this.tracks = [];
    for (var i = 0; i < numtracks; i++) {
        this.tracks.push(new $$.Stack());
    }
};

$$.Sequence.prototype.draw = function(ctx, bounds) {
    for (var i = 0; i < this.tracks.length; i++) {
        let dy = (bounds.y1 - bounds.y0)/this.tracks.length;
        this.tracks[i].draw(
            ctx, 
            new $$.Grid(bounds.x0, 
                        bounds.y0 + i*dy,
                        bounds.x1 / 100,
                        bounds.y0 + (i+1)*dy));

    }
};


return $$;

};

