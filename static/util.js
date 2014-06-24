util = {}

util.jsonGet = function(url, cb) {
    var xhr = new XMLHttpRequest();
    xhr.onload = function() {
        if (this.status != 200) {
            cb(null);
        } else {
            var json = null;
            try {
                json = JSON.parse(this.responseText);
            } catch(e) {
                json = null;
            }
            cb(json);
        }
    };
    xhr.open("get", url, true);
    xhr.send();
};

util.html = function(tag, attrs, style, contents) {
    return "<" + tag + " " + _.concat(_.map(attrs, function(a) { return a[0] + "=" + a[1] + " "; }))
               + 'style="' + _.concat(_.map(style, function(a) { return a[0] + ": " + a[1] + "; "; })) + '">'
               + contents + "</" + tag + ">";
};

_.concat = function(list) {
    if (list.length === 0) {
        return [];
    } else {
        return _.foldl(list, function(a, b) { return a.concat(b); });
    }
};
