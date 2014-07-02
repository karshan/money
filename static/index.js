commonStyle = [ ["height", "18px"]
              , ["visibility", "visible"]
              , ["pointer-events", "auto"]
              , ["text-align", "left"]
              ];

function sDiv(a, s) {
    a = a || [];
    s = s || [];
    return util.html("div", a, s, "&nbsp;");
}

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

function showAmount(a) {
    return (a/100) + "";
}

function showTags(tags) {
    return "[" + _.concat(_.intersperse(", ", tags)) + "]";
}

function showMonth(m) {
    return [ "Jan"
           , "Feb"
           , "Mar"
           , "Apr"
           , "May"
           , "Jun"
           , "Jul"
           , "Aug"
           , "Sep"
           , "Oct"
           , "Nov"
           , "Dec"
           ][m - 1]
}

function monthYear(d) {
    return showMonth(parseInt(d.substr(0, 2))) + " " + d.substr(6,8);
}

window.onload = function() {
    function renderNavBar() {
        return wDiv(1920, _.concat([ fwDiv(100, "Overview", [['onclick', 'state(["overview"]);']], [['cursor', 'pointer']])
                                   , fwDiv(100, "TList", [['onclick', 'state(["tlist"]);']], [['cursor', 'pointer']])
                                   ]), [], [['margin-bottom', '10px']]);
    }
    $('#navbar').html(renderNavBar());

    state(["tlist"]);
};

var TList = $R.state(null);
var monthStats = $R.state(null);
var similarTS = $R.state(null);
var editT = $R.state(null);

var state = $R(function(s) {
    var nop = function () {};

    util.clear();
    if (s[0] == "tlist") { 
        util.jsonGet("/transactions", TList);
        util.showPage("transaction_list", nop);
    }
    else if (s[0] == "edit") {
        util.jsonPost("/similar", editT(), similarTS);
        util.showPage("edit", nop);
    } else if (s[0] == "overview") {
        util.jsonGet("/monthStats", monthStats);
        util.showPage("overview", nop);
    }
});

var renderOverview = function(amountByTag) {
    var rendered = _.concatMap(_.filter(_.pairs(amountByTag), function(a) { return a[1] !== 0; }), function(a) {
        return wDiv(1920, _.concat([ fwDiv(500, showTags([a[0]]))
                                   , fwDiv(500, showAmount(a[1]))
                                   ]), [], []);
    });
    return   util.html("div", [], [], "Median monthly expenditure")
           + util.html("div", [], [], rendered);
};
$R(function(a) {
    $('#overview').html(renderOverview(a));
}).bindTo(monthStats);


var renderTList = function(ts) {
    var renderedts = _.concatMap(ts, function(t) {
        return wDiv(1920, _.concat([ fwDiv(100, t.date)
                                   , fwDiv(1000, t.description, [['onclick', 'trTagClick("' + btoa(JSON.stringify(t)) + '")']], [['cursor', 'pointer']])
                                   , fwDiv(100, showAmount(t.amount))
                                   , fwDiv(500, showTags(t.tags))
                                   ]));
    });
    return   util.html("div", [], [], "Balance: " + showAmount(_.sum(_.map(ts, function(a) { return a.amount; }))))
           + util.html("div", [], [], renderedts)
};
$R(function(ts) {
    $('#transaction_list').html(renderTList(ts));
}).bindTo(TList);
var trTagClick = util.jsonFunc(function(t) {
    editT(t);
    state(["edit"]);
});

var renderEdit = function(t, ts) {
    // renderedts :: String
    var renderedts = _.concat(_.map(ts, function(a) {
        var t = a[1];
        return wDiv(1920, _.concat([ fwDiv(100, t.date)
                                   , fwDiv(1000, t.description, [['onclick', 'editTrClick("' + btoa(JSON.stringify(t)) + '")']], [['cursor', 'pointer']])
                                   , fwDiv(100, showAmount(t.amount))
                                   , fwDiv(100, showTags(t.tags))
                                   ]));
    }));
    return   util.html("div", [], [], renderedts)
           + util.html("input", [["type", "text"], ["id", "edit_tag_in"]], [], "")
           + util.html("button", [['onclick', util.jsonOnclick('editSubmitClick', "")]], [], "submit");
};
$R(function(t, ts) {
    $('#edit').html(renderEdit(t, ts));
}).bindTo(editT, similarTS);
var editTrClick = util.jsonFunc(function(t) {
    similarTS(_.takeWhile(similarTS(), function(a) { return !_.isEqual(a[1], t) }));
});
var editSubmitClick = util.jsonFunc(function() {
    tags = $('#edit_tag_in').val();
    util.jsonPost("/updateTags", [_.map(similarTS(), function(a) { return a[1]; }), tags], function() { alert('done'); });
});
