
function readFile(path) {
    
    var text = "", file = new XMLHttpRequest();
    
    file.open("GET", path, false);
    
    file.onreadystatechange = function () {
        if (file.readyState == 4) {
            if (file.status == 200) {
                text = file.responseText;
            }
        }
    };
    
    file.send(null);
    
    return text;
}

// All scriptning som ska gras i bröjan placeras här
function init() {

	var content = document.getElementById("text");

    content.innerHTML = "Javascript: OK";
    content.innerHTML += readFile("readfile.txt");
}