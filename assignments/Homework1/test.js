var myNum = 4;
var myFun = function(x) {
    myNum = 2;
    return(myNum * yourFun(x));
}

var yourFun = function(x) {
    return x * myNum;
}

console.(myFun(4))
