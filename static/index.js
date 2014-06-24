commonStyle = [ ["height", "18px"]
              , ["padding", "0px"]
              , ["margin", "0px"]
              , ["visibility", "visible"]
              , ["pointer-events", "auto"]
              , ["text-align", "left"]
              ];

function fwDiv(w, c) {
    return util.html("div", [], [["width", w + "px"], ["float", "left"]].concat(commonStyle), c);
}

function wDiv(w, c) {
    return util.html("div", [], [["width", w + "px"]].concat(commonStyle), c);
}

window.onload = function() {
    util.jsonGet("/transactions", function(ts) {
        TList(ts); // TODO handle null error
    });
}

var TList = $R(function(a) { return a; });

$R(function (ts) { // renderTList
    // renderedts :: [String]
    var renderedts = _.map(ts, function(t) {
        return wDiv(1920, _.concat([ fwDiv(100, t.date)
                                   , fwDiv(1000, t.description)
                                   , fwDiv(100, t.amount)
                                   , fwDiv(100, JSON.stringify(t.tags))
                                   ]));
    });
    $('#transaction_list').html(renderedts);
}).bindTo(TList)
