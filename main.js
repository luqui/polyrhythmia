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
$$.Grid = function() {
};

$$.Grid.prototype.locateX = function(x) {
    return 10*x;
};

$$.Grid.prototype.locateY = function(y) {
    return 100*y;
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
    console.log(x0, y0, x1-x0, y1-y0);
    ctx.fillRect(x0, y0, x1 - x0, y1 - y0);
};

return $$;

};

