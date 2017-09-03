MidiModule = function() {

var $$ = {};

$$.open = function(navigator, callback) {
    if (!navigator.requestMIDIAccess) {
        alert("No MIDI support in browser");
    }

    var onsuccess = function(midiobj) { 
        var first = null;
        midiobj.outputs.forEach(function(out) {
            if (!first) { first = out; }
        });
        console.log("Opening output device ", first.name);
        first.open().then(function(device) {
            callback(new $$.OutDevice(device));
        });
    };
    var onfailure = function() {
        alert("MIDI initialization failed");
    };

    navigator.requestMIDIAccess().then(onsuccess, onfailure);
};

$$.OutDevice = function(dev) {
    this._dev = dev;
};

$$.OutDevice.prototype.noteOn = function(chan, note, vel) {
    if (!(0 <= chan < 16)) {
        throw("Invalid Channel " + chan);
    }
    if (!(0 <= note < 128)) {
        throw("Invalid Note " + note);
    }
    if (!(0 <= vel < 128)) {
        throw("Invalid Velocity " + vel);
    }
    this._dev.send([0x90 + chan, note, vel]);
};

$$.OutDevice.prototype.noteOff = function(chan, note) {
    this.noteOn(chan, note, 0);
};

return $$;

};
