commonStyle = [ ["height", "18px"]
              , ["padding", "0px"]
              , ["margin", "0px"]
              , ["visibility", "visible"]
              , ["pointer-events", "auto"]
              , ["text-align", "left"]
              ];

function fwDiv(w, c, a, s) {
    a = a || [];
    s = s || [];
    return util.html("div", a, [["width", w + "px"], ["float", "left"]].concat(commonStyle).concat(s), c);
}

function wDiv(w, c, a, s) {
    a = a || [];
    s = s || [];
    return util.html("div", a, [["width", w + "px"]].concat(commonStyle).concat(s), c);
}

window.onload = function() {
    util.jsonGet("/transactions", function(ts) {
        TList(ts); // TODO handle null error
    });
}

var TList = $R(function(a) { return a; });

function renderTList(ts) {
    function showAmount(a) {
        return (a/100) + "";
    }

    // renderedts :: [String]
    var renderedts = _.map(ts, function(t) {
        return wDiv(1920, _.concat([ fwDiv(100, t.date)
                                   , fwDiv(1000, t.description)
                                   , fwDiv(100, showAmount(t.amount))
                                   , fwDiv(100, JSON.stringify(t.tags), [['onclick', 'trTagClick("' + btoa(JSON.stringify(t)) + '")']])
                                   ]));
    });
    $('#transaction_list').html(renderedts);
}
$R(renderTList).bindTo(TList)

// balance :: Signal Number
var balance = $R(function(ts) {
    return _.foldl(ts, function(s, e) { return s + e.amount; }, 0);
}).bindTo(TList);
