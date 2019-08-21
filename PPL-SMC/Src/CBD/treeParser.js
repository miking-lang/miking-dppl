function Queue(){var a=[],b=0;this.getLength=function(){return a.length-b};this.isEmpty=function(){return 0==a.length};this.enqueue=function(b){a.push(b)};this.dequeue=function(){if(0!=a.length){var c=a[b];2*++b>=a.length&&(a=a.slice(b),b=0);return c}};this.peek=function(){return 0<a.length?a[b]:void 0}};

var treeSimple = '{"right":{"right":{"age":0}, "left":{"age":0}, "age":4}, "left":{"right":{"age":0}, "left":{"age":0},"age":6}, "age":10}';
var treeMonkeys = '{"right":{"right":{"right":{"right":{"age":0.000000},"left":{"age":0.000000},"age":2.600000}, "left":{"age":0.000000},"age":4.432900}, "left":{"right":{"right":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":2.096700},"age":4.888820},"left":{"age":0.000000},"age":8.340000},"left":{"right":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":1.330000},"age":2.810000},"age":4.340400},"age":5.692320},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":1.720000},"age":2.239800},"age":3.045170},"age":6.135400},"age":10.237967},"age":11.016198},"left":{"right":{"right":{"right":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":2.994200},"age":3.283800},"age":5.900000},"left":{"age":0.000000},"age":7.110000},"left":{"right":{"right":{"age":0.000000},"left":{"right":{"age":0.000000},"left":{"age":0.000000},"age":0.022830},"age":1.107800},"left":{"age":0.000000},"age":3.196940},"age":7.310000},"left":{"right":{"right":{"age":0.000000},"left":{"age":0.000000},"age":2.957100},"left":{"age":0.000000},"age":5.485300},"age":11.330548},"age":17.993216}';
var treeBisse = '{"age":13.016,"left":{"age":10.626,"left":{"age":8.352,"left":{"age":7.679,"left":{"age":5.187,"left":{"age":0},"right":{"age":0}},"right":{"age":5.196,"left":{"age":0},"right":{"age":4.871,"left":{"age":2.601,"left":{"age":0},"right":{"age":0}},"right":{"age":0}}}},"right":{"age":7.361,"left":{"age":3.818,"left":{"age":1.143,"left":{"age":0.0829,"left":{"age":0},"right":{"age":0}},"right":{"age":0}},"right":{"age":1.813,"left":{"age":0.0452,"left":{"age":0.0203,"left":{"age":0},"right":{"age":0}},"right":{"age":0}},"right":{"age":0}}},"right":{"age":1.868,"left":{"age":0.0866,"left":{"age":0},"right":{"age":0.0001,"left":{"age":0},"right":{"age":0}}},"right":{"age":1.06,"left":{"age":0},"right":{"age":0}}}}},"right":{"age":10.536,"left":{"age":8.291,"left":{"age":1.396,"left":{"age":0.0215,"left":{"age":0},"right":{"age":0}},"right":{"age":0}},"right":{"age":0}},"right":{"age":8.192,"left":{"age":0.056,"left":{"age":0},"right":{"age":0}},"right":{"age":0}}}},"right":{"age":8.958,"left":{"age":3.748,"left":{"age":0},"right":{"age":0.0033,"left":{"age":0},"right":{"age":0}}},"right":{"age":7.775,"left":{"age":0.0584,"left":{"age":0},"right":{"age":0}},"right":{"age":1.589,"left":{"age":0},"right":{"age":0}}}}}';

var tree = JSON.parse(treeBisse);

console.log(tree);

// console.log(tree.right)

var idxCounter = 0;
// var fifo = [];
var fifo = new Queue();

var ages = [];
var leftIdx = [];
var rightIdx = [];

function bfsIdx(tree) {
    if(tree.left != null)
        fifo.enqueue(tree.left);
    if(tree.right != null)
        fifo.enqueue(tree.right);

    var numNull = 0;
    if(tree.left == null) numNull++;
    if(tree.right == null) numNull++;
    if(numNull == 1)
        console.log("hori sheet, only one child");

    // Add index
    tree.idx = idxCounter;
    idxCounter++;

    if(! fifo.isEmpty()) {
        var subTree = fifo.dequeue();
        bfsIdx(subTree);
    }
}

function bfsArrs(tree) {
    if(tree.left != null)
        fifo.enqueue(tree.left);
    if(tree.right != null)
        fifo.enqueue(tree.right);

    // Build arrays
    ages.push(tree.age);
    leftIdx.push(tree.left == null ? -1 : tree.left.idx);
    rightIdx.push(tree.right == null ? -1 : tree.right.idx);

    if(! fifo.isEmpty()) {
        var subTree = fifo.dequeue();
        bfsArrs(subTree);
    }
}

function transformTree() {
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


