var fs = require("fs");

var app = Elm.Main.init()

app.ports.toJs.subscribe(function(fromElm) {
    if (fromElm.type_ == "readFile") {
        fs.readFile(fromElm.value, "utf-8", function(err, data) {
            app.ports.fromJs.send({
                type_: "file_contents",
                value: {
                    path: fromElm.value,
                    contents: data,
                    err: err}});
        })
    }
});

// var fs = require("fs")
// 
// fs.readFile("elm.json", "utf-8", function(err, data) {
//     
// })
