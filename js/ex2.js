$(document).ready(function() {
    $("button").click(function () {
        $(this).hide();
        $("#red").css("width", "100%");
        $("table").first().css("height", "50px");
        $("table:eq(1)").css("min-height", "500px");
        $("table:eq(1)").css("position", "relative");
        $("table:eq(1)").css("bottom", "30px");
        $("#green div").replaceWith("<h1> Hello World </h1>");
        $("h1").css("color","yellow");
        $("h1").css("position","absolute");
        $("h1").css("top","15px");
        $("h1").css("padding-left","210px");
        $("blue").css("border-radius","50%");
        $("blue").css("width","50px");
    })
});
