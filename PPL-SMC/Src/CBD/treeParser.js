function Queue(){var a=[],b=0;this.getLength=function(){return a.length-b};this.isEmpty=function(){return 0==a.length};this.enqueue=function(b){a.push(b)};this.dequeue=function(){if(0!=a.length){var c=a[b];2*++b>=a.length&&(a=a.slice(b),b=0);return c}};this.peek=function(){return 0<a.length?a[b]:void 0}};

var treeSimple = '{"right":{"right":{"age":0}, "left":{"age":0}, "age":4}, "left":{"right":{"age":0}, "left":{"age":0},"age":6}, "age":10}';
var treeMonkeys = '{"right":{"right":{"right":{"right":{"age":0.000000},"left":{"age":0.000000},"age":2.600000}, "left":{"age":0.000000},"age":4.432900}, "left":{"right":{"right":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":2.096700},"age":4.888820},"left":{"age":0.000000},"age":8.340000},"left":{"right":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":1.330000},"age":2.810000},"age":4.340400},"age":5.692320},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":1.720000},"age":2.239800},"age":3.045170},"age":6.135400},"age":10.237967},"age":11.016198},"left":{"right":{"right":{"right":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":2.994200},"age":3.283800},"age":5.900000},"left":{"age":0.000000},"age":7.110000},"left":{"right":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":0.022830},"age":1.107800},"left":{"age":0.000000},"age":3.196940},"age":7.310000},"left":{"right":{"right":{"age":0.000000},"left":{"age":0.000000},"age":2.957100},"left":{"age":0.000000},"age":5.485300},"age":11.330548},"age":17.993216}';
var treeBisse = '{"age":13.016,"left":{"age":10.626,"left":{"age":8.352,"left":{"age":7.679,"left":{"age":5.187,"left":{"age":0},"right":{"age":0}},"right":{"age":5.196,"left":{"age":0},"right":{"age":4.871,"left":{"age":2.601,"left":{"age":0},"right":{"age":0}},"right":{"age":0}}}},"right":{"age":7.361,"left":{"age":3.818,"left":{"age":1.143,"left":{"age":0.0829,"left":{"age":0},"right":{"age":0}},"right":{"age":0}},"right":{"age":1.813,"left":{"age":0.0452,"left":{"age":0.0203,"left":{"age":0},"right":{"age":0}},"right":{"age":0}},"right":{"age":0}}},"right":{"age":1.868,"left":{"age":0.0866,"left":{"age":0},"right":{"age":0.0001,"left":{"age":0},"right":{"age":0}}},"right":{"age":1.06,"left":{"age":0},"right":{"age":0}}}}},"right":{"age":10.536,"left":{"age":8.291,"left":{"age":1.396,"left":{"age":0.0215,"left":{"age":0},"right":{"age":0}},"right":{"age":0}},"right":{"age":0}},"right":{"age":8.192,"left":{"age":0.056,"left":{"age":0},"right":{"age":0}},"right":{"age":0}}}},"right":{"age":8.958,"left":{"age":3.748,"left":{"age":0},"right":{"age":0.0033,"left":{"age":0},"right":{"age":0}}},"right":{"age":7.775,"left":{"age":0.0584,"left":{"age":0},"right":{"age":0}},"right":{"age":1.589,"left":{"age":0},"right":{"age":0}}}}}';
var treeCetacean  = '{ "age": 40 , "left": { "age": 35.4248 , "left": { "age": 27.8341 , "left": { "age": 8.57687 , "left": { "age": 0 }, "right": { "age": 1.27866 , "left": { "age": 0 }, "right": { "age": 0.188389 , "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 25.8432 , "left": { "age": 0 }, "right": { "age": 18.0947 , "left": { "age": 0 }, "right": { "age": 16.169 , "left": { "age": 15.2139 , "left": { "age": 5.36058 , "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 10.6031 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 12.9642 , "left": { "age": 0 }, "right": { "age": 11.4896 , "left": { "age": 0 }, "right": { "age": 5.34343 , "left": { "age": 0 }, "right": { "age": 4.38115 , "left": { "age": 0 }, "right": { "age": 0 } } } } } } } } }, "right": { "age": 33.4157 , "left": { "age": 22.167 , "left": { "age": 0 }, "right": { "age": 8.85313 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 32.3769 , "left": { "age": 0.335479 , "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 31.7146 , "left": { "age": 19.0147 , "left": { "age": 0 }, "right": { "age": 17.8581 , "left": { "age": 6.34314 , "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 15.5499 , "left": { "age": 0 }, "right": { "age": 14.393 , "left": { "age": 11.0056 , "left": { "age": 0 }, "right": { "age": 8.05967 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 12.9165 , "left": { "age": 0 }, "right": { "age": 11.0576 , "left": { "age": 0 }, "right": { "age": 8.94251 , "left": { "age": 0 }, "right": { "age": 8.23509 , "left": { "age": 0 }, "right": { "age": 7.71938 , "left": { "age": 0 }, "right": { "age": 7.20224 , "left": { "age": 4.77541 , "left": { "age": 0 }, "right": { "age": 4.19742 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 6.36511 , "left": { "age": 0 }, "right": { "age": 5.78569 , "left": { "age": 4.89284 , "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 5.07855 , "left": { "age": 0 }, "right": { "age": 4.11628 , "left": { "age": 0 }, "right": { "age": 0 } } } } } } } } } } } } } } }, "right": { "age": 25.7272 , "left": { "age": 24.3338 , "left": { "age": 0 }, "right": { "age": 18.2519 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 17.5865 , "left": { "age": 13.6965 , "left": { "age": 5.12629 , "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 5.30772 , "left": { "age": 4.60801 , "left": { "age": 0 }, "right": { "age": 3.71002 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 4.66373 , "left": { "age": 0 }, "right": { "age": 3.38146 , "left": { "age": 0 }, "right": { "age": 0 } } } } }, "right": { "age": 10.5348 , "left": { "age": 0 }, "right": { "age": 9.37282 , "left": { "age": 8.1714 , "left": { "age": 0 }, "right": { "age": 5.99285 , "left": { "age": 0 }, "right": { "age": 5.41429 , "left": { "age": 0 }, "right": { "age": 4.38744 , "left": { "age": 0 }, "right": { "age": 2.97717 , "left": { "age": 0 }, "right": { "age": 1.4389 , "left": { "age": 0 }, "right": { "age": 0 } } } } } } }, "right": { "age": 8.65426 , "left": { "age": 0 }, "right": { "age": 8.06569 , "left": { "age": 6.86684 , "left": { "age": 0 }, "right": { "age": 5.19829 , "left": { "age": 1.29976 , "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 4.48887 , "left": { "age": 3.21317 , "left": { "age": 0 }, "right": { "age": 1.73918 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 3.71631 , "left": { "age": 0 }, "right": { "age": 2.81775 , "left": { "age": 0 }, "right": { "age": 1.98318 , "left": { "age": 0 }, "right": { "age": 1.46919 , "left": { "age": 0 }, "right": { "age": 0 } } } } } } } }, "right": { "age": 7.40827 , "left": { "age": 5.81486 , "left": { "age": 0 }, "right": { "age": 3.06089 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 4.26573 , "left": { "age": 3.31117 , "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 3.49202 , "left": { "age": 2.92089 , "left": { "age": 0 }, "right": { "age": 2.0869 , "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 2.71917 , "left": { "age": 0 }, "right": { "age": 1.81976 , "left": { "age": 0 }, "right": { "age": 1.36833 , "left": { "age": 0.859479 , "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 1.10919 , "left": { "age": 0 }, "right": { "age": 0.786909 , "left": { "age": 0 }, "right": { "age": 0 } } } } } } } } } } } } } } } } } } }, "right": null }';
var treePrimates122 = '{ "age": 0.11, "left": { "age": 0.106195, "left": { "age": 0 }, "right": { "age": 0.101094, "left": { "age": 0.0958959, "left": { "age": 0.0905502, "left": { "age": 0 }, "right": { "age": 0.0586295, "left": { "age": 0.0401652, "left": { "age": 0.0379268, "left": { "age": 0 }, "right": { "age": 0.0332042, "left": { "age": 0 }, "right": { "age": 0.0301782, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 0.0392226, "left": { "age": 0.0346577, "left": { "age": 0 }, "right": { "age": 0.0327925, "left": { "age": 0 }, "right": { "age": 0.032125, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 0.0365468, "left": { "age": 0.0303611, "left": { "age": 0 }, "right": { "age": 0.0295462, "left": { "age": 0 }, "right": { "age": 0.026754, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 0.0361695, "left": { "age": 0 }, "right": { "age": 0.0342505, "left": { "age": 0 }, "right": { "age": 0 } } } } } }, "right": { "age": 0.0512462, "left": { "age": 0.0474721, "left": { "age": 0.0411942, "left": { "age": 0 }, "right": { "age": 0.0408786, "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 0.0460226, "left": { "age": 0 }, "right": { "age": 0.042394, "left": { "age": 0 }, "right": { "age": 0.0418415, "left": { "age": 0 }, "right": { "age": 0 } } } } }, "right": { "age": 0.0427258, "left": { "age": 0.0399164, "left": { "age": 0.0381464, "left": { "age": 0 }, "right": { "age": 0.0377291, "left": { "age": 0.0363782, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 0.0371656, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 0.038921, "left": { "age": 0.0378658, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 0.0388686, "left": { "age": 0 }, "right": { "age": 0.0379197, "left": { "age": 0 }, "right": { "age": 0 } } } } }, "right": { "age": 0.0404512, "left": { "age": 0.0392285, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 0.0394287, "left": { "age": 0 }, "right": { "age": 0.0392993, "left": { "age": 0.0366504, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 0.0386101, "left": { "age": 0 }, "right": { "age": 0.0384165, "left": { "age": 0 }, "right": { "age": 0 } } } } } } } } } }, "right": { "age": 0.0827704, "left": { "age": 0.0783974, "left": { "age": 0 }, "right": { "age": 0.0659509, "left": { "age": 0.0597016, "left": { "age": 0 }, "right": { "age": 0.0559496, "left": { "age": 0 }, "right": { "age": 0.0449837, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 0.0647547, "left": { "age": 0.0516022, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 0.0612976, "left": { "age": 0 }, "right": { "age": 0.0571017, "left": { "age": 0 }, "right": { "age": 0.0465375, "left": { "age": 0 }, "right": { "age": 0 } } } } } } }, "right": { "age": 0.0319856, "left": { "age": 0.0293809, "left": { "age": 0.014042, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 0.0203923, "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 0.013504, "left": { "age": 0 }, "right": { "age": 0 } } } } }, "right": { "age": 0.0480665, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": null }';
var treePlacental = '{ "age": 110, "left": { "age": 102.892, "left": { "age": 98.9037, "left": { "age": 88.7079, "left": { "age": 86.594, "left": { "age": 85.7355, "left": { "age": 83.6305, "left": { "age": 78.8159, "left": { "age": 0 }, "right": { "age": 56.2941, "left": { "age": 45.9463, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 11.2945, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 55.9447, "left": { "age": 0 }, "right": { "age": 45.4221, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 66.2414, "left": { "age": 49.7958, "left": { "age": 41.528, "left": { "age": 0 }, "right": { "age": 16.5314, "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 44.3141, "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 58.2641, "left": { "age": 0 }, "right": { "age": 18.0068, "left": { "age": 0 }, "right": { "age": 0 } } } } }, "right": { "age": 64.5378, "left": { "age": 60.9777, "left": { "age": 0 }, "right": { "age": 55.6027, "left": { "age": 11.7401, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 52.3518, "left": { "age": 0 }, "right": { "age": 32.7746, "left": { "age": 0 }, "right": { "age": 0 } } } } }, "right": { "age": 5.59271, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 79.4479, "left": { "age": 0 }, "right": { "age": 76.1405, "left": { "age": 66.6659, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 37.0936, "left": { "age": 0 }, "right": { "age": 0 } } } } }, "right": { "age": 91.2628, "left": { "age": 90.0779, "left": { "age": 88.1646, "left": { "age": 79.677, "left": { "age": 75.7331, "left": { "age": 0 }, "right": { "age": 41.2056, "left": { "age": 0 }, "right": { "age": 29.234, "left": { "age": 0 }, "right": { "age": 17.0624, "left": { "age": 0 }, "right": { "age": 9.86572, "left": { "age": 0 }, "right": { "age": 8.06044, "left": { "age": 0 }, "right": { "age": 0 } } } } } } }, "right": { "age": 63.1203, "left": { "age": 0 }, "right": { "age": 41.7407, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 16.6128, "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 61.4772, "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 86.0406, "left": { "age": 75.4367, "left": { "age": 72.3881, "left": { "age": 67.7464, "left": { "age": 57.8912, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 63.6626, "left": { "age": 0 }, "right": { "age": 14.7487, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 37.1389, "left": { "age": 0 }, "right": { "age": 26.1162, "left": { "age": 0 }, "right": { "age": 13.0395, "left": { "age": 0 }, "right": { "age": 0 } } } } }, "right": { "age": 67.2494, "left": { "age": 0 }, "right": { "age": 27.0262, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 53.3116, "left": { "age": 0 }, "right": { "age": 17.857, "left": { "age": 0 }, "right": { "age": 0 } } } } } }, "right": { "age": 98.7513, "left": { "age": 72.0842, "left": { "age": 69.4843, "left": { "age": 0 }, "right": { "age": 66.6285, "left": { "age": 59.6655, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 14.8617, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 58.3762, "left": { "age": 0 }, "right": { "age": 53.5975, "left": { "age": 0 }, "right": { "age": 0 } } } }, "right": { "age": 69.872, "left": { "age": 41.1423, "left": { "age": 0 }, "right": { "age": 8.92313, "left": { "age": 0 }, "right": { "age": 0 } } }, "right": { "age": 59.1813, "left": { "age": 10.9256, "left": { "age": 0 }, "right": { "age": 0 } }, "right": { "age": 12.7694, "left": { "age": 0 }, "right": { "age": 0 } } } } } }, "right": null }';

var tree = JSON.parse(treeCetacean);

console.log(tree);

// console.log(tree.right)

var idxCounter = 0;
// var fifo = [];
var fifo = new Queue();

var ages = [];
var leftIdx = [];
var rightIdx = [];
var parentIdx = [];

function bfsIdx(tree) {
    if(tree.left != null) {
        fifo.enqueue(tree.left);
        tree.left.parentIdx = idxCounter;
    }
    if(tree.right != null) {
        fifo.enqueue(tree.right);
        tree.right.parentIdx = idxCounter;
    }

    var numNull = 0;
    if(tree.left == null) numNull++;
    if(tree.right == null) numNull++;
    if(numNull == 1)
        alert("hori sheet, only one child");

    // Add index
    tree.idx = idxCounter;
    idxCounter++;

    if(! fifo.isEmpty()) {
        var subTree = fifo.dequeue();
        bfsIdx(subTree);
    }
}

function bfsArrs(tree) {
    if(tree.left != null) {
        fifo.enqueue(tree.left);
    }
    if(tree.right != null)
        fifo.enqueue(tree.right);

    // Build arrays
    ages.push(tree.age);
    leftIdx.push(tree.left == null ? -1 : tree.left.idx);
    rightIdx.push(tree.right == null ? -1 : tree.right.idx);
    parentIdx.push(tree.parentIdx);

    if(! fifo.isEmpty()) {
        var subTree = fifo.dequeue();
        bfsArrs(subTree);
    }
}

function transformTree() {
    tree.parentIdx = -1;
    bfsIdx(tree);
    bfsArrs(tree);

    var str = JSON.stringify(tree, null, 32);
    console.log(str);

    console.log("\nAges:");
    console.log(ages);
    console.log("\nleftIdx:");
    console.log(leftIdx);
    console.log("\nrightIdx:");
    console.log(rightIdx);    
    console.log("\nparentIdx:");
    console.log(parentIdx);    
}

function max(a, b) {
    return a >= b ? a : b;
}

function treeMaxDepth(tree) {
    if(tree.left == null && tree.right == null) {
        return 1;
    }

    var lDepth = tree.left == null ? 0 : treeMaxDepth(tree.left);
    var rDepth = tree.right == null ? 0 : treeMaxDepth(tree.right);

    return max(lDepth, rDepth) + 1;

}


transformTree();

console.log("\nMax tree depth: ");
console.log(treeMaxDepth(tree));


// From the webppl represantation
function convertTreeToJSON() {

    var leaf = function( a, i ) {
        return { age: a}
    }

    var node = function( a, l, r ) {
        return { age: a, left: l, right: r }
    }

    var tree = node(110,node(102.892,node(98.9037,node(88.7079,node(86.594,node(85.7355,node(83.6305,node(78.8159,leaf(0),node(56.2941,node(45.9463,leaf(0),leaf(0)),node(11.2945,leaf(0),leaf(0)))),node(55.9447,leaf(0),node(45.4221,leaf(0),leaf(0)))),node(66.2414,node(49.7958,node(41.528,leaf(0),node(16.5314,leaf(0),leaf(0))),node(44.3141,leaf(0),leaf(0))),node(58.2641,leaf(0),node(18.0068,leaf(0),leaf(0))))),node(64.5378,node(60.9777,leaf(0),node(55.6027,node(11.7401,leaf(0),leaf(0)),node(52.3518,leaf(0),node(32.7746,leaf(0),leaf(0))))),node(5.59271,leaf(0),leaf(0)))),node(79.4479,leaf(0),node(76.1405,node(66.6659,leaf(0),leaf(0)),node(37.0936,leaf(0),leaf(0))))),node(91.2628,node(90.0779,node(88.1646,node(79.677,node(75.7331,leaf(0),node(41.2056,leaf(0),node(29.234,leaf(0),node(17.0624,leaf(0),node(9.86572,leaf(0),node(8.06044,leaf(0),leaf(0))))))),node(63.1203,leaf(0),node(41.7407,leaf(0),leaf(0)))),node(16.6128,leaf(0),leaf(0))),node(61.4772,leaf(0),leaf(0))),node(86.0406,node(75.4367,node(72.3881,node(67.7464,node(57.8912,leaf(0),leaf(0)),node(63.6626,leaf(0),node(14.7487,leaf(0),leaf(0)))),node(37.1389,leaf(0),node(26.1162,leaf(0),node(13.0395,leaf(0),leaf(0))))),node(67.2494,leaf(0),node(27.0262,leaf(0),leaf(0)))),node(53.3116,leaf(0),node(17.857,leaf(0),leaf(0)))))),node(98.7513,node(72.0842,node(69.4843,leaf(0),node(66.6285,node(59.6655,leaf(0),leaf(0)),node(14.8617,leaf(0),leaf(0)))),node(58.3762,leaf(0),node(53.5975,leaf(0),leaf(0)))),node(69.872,node(41.1423,leaf(0),node(8.92313,leaf(0),leaf(0))),node(59.1813,node(10.9256,leaf(0),leaf(0)),node(12.7694,leaf(0),leaf(0)))))),null);
    console.log(tree);

    var str = JSON.stringify(tree, null, 32)
    console.log(str)
}

// convertTreeToJSON();
