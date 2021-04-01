/**
 * File tree-parser.js contains a bunch of functions to parse JSON trees into arrays, and give specs about trees.
 *
 * Can be executed online in the browser, e.g.: https://playcode.io/.
 * 
 * Or, use node
 *
 *   node tree-parser.js TREEFILE STRUCTNAME
 *
 * where
 *   TREEFILE is the fully qualified path to a PhyJSON file, containing a tree.
 *   STRUCTNAME is the (prefix of the) name of the generated datastrcuture.
 *
 * The output is written on the standard output.
 *
 * Example:
 *
 *   node tree-parser.js /home/viktor/ownCloud/probabilistic-programming/data/bisse_32.phyjson
 */

// Configuration, change if needed
const phyjs = require("/home/viktor/ownCloud/probabilistic-programming/webppl/phyjs/index.js");

// Queue implementation (library)
function Queue(){var a=[],b=0;this.getLength=function(){return a.length-b};this.isEmpty=function(){return 0==a.length};this.enqueue=function(b){a.push(b)};this.dequeue=function(){if(0!=a.length){var c=a[b];2*++b>=a.length&&(a=a.slice(b),b=0);return c}};this.peek=function(){return 0<a.length?a[b]:void 0}};

var tree = phyjs.read_phyjson(process.argv[2]);

//var tree = JSON.parse(treeMothDiv);
//console.log(tree);
// console.log(tree.right)

var idxCounter = 0;
// var fifo = [];
var fifo = new Queue();
var stack = [];

var ages = [];
var leftIdx = [];
var rightIdx = [];
var parentIdx = [];
var nextIdx;

// Breadth First Search that assigns pointers in the tree. 
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
        alert("Caution: Only one child (unbalanced tree?)");

    // Add index
    tree.idx = idxCounter;
    idxCounter++;

    if(! fifo.isEmpty()) {
        var subTree = fifo.dequeue();
        bfsIdx(subTree);
    }
}

// Breadth First Search that fills the arrays with indices. Called after bfsIdx has been called. 
function bfsArrs(tree) {
    if(tree.left != null) 
        fifo.enqueue(tree.left);

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

// Depth First Search that fills the array of next pointers (pre-processed tree traversal path)
function dfsNext(tree) {
    if(tree.right != null)
        stack.push(tree.right);

    if(tree.left != null)
        stack.push(tree.left);

    if(stack.length == 0) {
        nextIdx[tree.idx] = -1;
        return;
    }

    var nextTree = stack.pop();
    
    nextIdx[tree.idx] = nextTree.idx;
    dfsNext(nextTree);
}

// The function that generates the transformed tree. JSON tree -> RootPPL SoA Tree
function transformTree() {
    tree.parentIdx = -1;
    bfsIdx(tree);
    bfsArrs(tree);

    nextIdx = new Array(ages.length);
    dfsNext(tree);

    var str = JSON.stringify(tree, null, 32);
    //console.log(str);

    console.log("struct %s_tree_t {", process.argv[3].split(".")[0]);
    console.log("\tstatic const int NUM_NODES = %d;", ages.length);
    console.log("\tstatic const int MAX_DEPTH = %d;", treeMaxDepth(tree));
    
    //console.log("\nAges:");
    //console.log(ages);
    console.log("\tconst floating_t ages[NUM_NODES] = ", JSON.stringify(ages, null, 0).replace("[", "{").replace("]", "}").concat(";"));
    
    //console.log("\nleftIdx:");
    //console.log(leftIdx);
    console.log("\tconst int idxLeft[NUM_NODES] = ", JSON.stringify(leftIdx, null, 0).replace("[", "{").replace("]", "}").concat(";"));
    
    //console.log("\nrightIdx:");
    //console.log(rightIdx);
    console.log("\tconst int idxRight[NUM_NODES] = ", JSON.stringify(rightIdx, null, 0).replace("[", "{").replace("]", "}").concat(";"));
    
    //console.log("\nparentIdx:");
    //console.log(parentIdx);
    console.log("\tconst int idxParent[NUM_NODES] = ", JSON.stringify(parentIdx, null, 0).replace("[", "{").replace("]", "}").concat(";"));
    
    //console.log("\nnextIdx:");
    //console.log(nextIdx);
    console.log("\tconst int idxNext[NUM_NODES] = ", JSON.stringify(nextIdx, null, 0).replace("[", "{").replace("]", "}").concat(";"));

    console.log("};\n");
}

function max(a, b) {
    return a >= b ? a : b;
}

// Finds the max depth of the tree
function treeMaxDepth(tree) {
    if(tree.left == null && tree.right == null) {
        return 1;
    }

    var lDepth = tree.left == null ? 0 : treeMaxDepth(tree.left);
    var rDepth = tree.right == null ? 0 : treeMaxDepth(tree.right);

    return max(lDepth, rDepth) + 1;

}


transformTree();

//console.log("\nMax tree depth: ");
//console.log(treeMaxDepth(tree));

// Takes WebPPL formatted tree (currently hardcoded in local variable tree) and creates WebPPL JSON object which is printed to stdout. 
function convertTreeToJSONWebPPLFormat() {
    // Tree stuff
    var leaf = function( a, i ) {
        return { type: 'leaf', age: a, index: i }
    }
    var node = function( a, l, r ) {
        return { type: 'node', age: a, left: l, right: r }
    }

    var tree = node(65.0917,node(51.8705,node(20.9224,node(8.5207,node(3.64498,leaf(0,102),node(1.44986,leaf(0,99),leaf(0,98))),node(7.32733,node(3.5741,leaf(0,106),leaf(0,105)),node(6.31619,node(5.40351,leaf(0,100),node(2.36434,node(1.46116,leaf(0,104),leaf(0,103)),leaf(0,101))),node(4.24264,leaf(0,167),leaf(0,166))))),node(16.3413,node(4.70363,leaf(0,171),node(1.88177,leaf(0,21),leaf(0,22))),node(9.20057,leaf(0,139),node(2.67248,leaf(0,165),leaf(0,164))))),node(39.4824,leaf(0,91),node(27.5162,node(22.0348,node(17.1722,node(11.1999,leaf(0,135),node(7.6009,leaf(0,137),node(5.02863,leaf(0,136),node(3.00293,leaf(0,133),node(1.36316,node(0.453228,leaf(0,132),leaf(0,134)),leaf(0,131)))))),node(13.0595,node(6.22907,leaf(0,126),node(5.66245,node(5.34291,leaf(0,108),leaf(0,110)),leaf(0,109))),node(11.8927,leaf(0,233),node(6.37658,node(3.07403,leaf(0,97),leaf(0,96)),node(3.75691,leaf(0,93),node(3.68402,leaf(0,95),leaf(0,94))))))),node(13.6168,leaf(0,29),node(7.43524,leaf(0,123),node(5.87983,leaf(0,192),node(1.54142,leaf(0,194),leaf(0,193)))))),node(17.0354,leaf(0,172),node(11.4707,node(7.05337,leaf(0,2),node(4.09979,leaf(0,158),node(2.73742,leaf(0,160),leaf(0,159)))),node(9.40047,leaf(0,83),leaf(0,82))))))),node(62.6001,node(35.1825,node(15.3009,node(0.257424,leaf(0,219),leaf(0,220)),leaf(0,221)),node(8.68355,leaf(0,222),leaf(0,218))),node(51.225,node(30.1522,node(25.3163,node(20.9726,node(11.8807,node(8.12242,node(3.68845,leaf(0,85),leaf(0,84)),node(3.70051,leaf(0,32),leaf(0,31))),node(10.2338,leaf(0,177),node(6.54958,leaf(0,175),node(3.78989,leaf(0,173),node(1.75563,leaf(0,176),leaf(0,174)))))),node(16.4714,leaf(0,45),node(13.303,node(7.32881,leaf(0,40),node(3.08581,node(0.655408,leaf(0,42),leaf(0,43)),leaf(0,37))),node(10.3035,leaf(0,44),node(8.00036,node(4.72442,leaf(0,38),node(1.98277,leaf(0,36),leaf(0,34))),node(5.32729,node(2.35055,leaf(0,39),leaf(0,33)),node(2.32372,leaf(0,41),leaf(0,35)))))))),node(20.6835,node(12.7299,node(0.502263,leaf(0,7),leaf(0,5)),node(6.92635,leaf(0,4),node(4.52731,leaf(0,8),node(2.9471,leaf(0,6),node(1.75869,node(0.793145,leaf(0,10),leaf(0,9)),leaf(0,3)))))),node(11.2865,node(4.69295,node(3.0384,leaf(0,28),node(2.03735,leaf(0,23),node(1.215,leaf(0,26),leaf(0,25)))),node(2.88507,leaf(0,27),leaf(0,24))),node(9.03084,leaf(0,30),node(2.92191,leaf(0,125),leaf(0,124)))))),node(25.6929,node(23.1866,node(7.26239,node(3.96378,leaf(0,59),leaf(0,57)),node(0.141184,leaf(0,58),leaf(0,56))),node(8.42087,leaf(0,212),node(6.82414,leaf(0,216),node(3.94301,leaf(0,214),node(1.76531,leaf(0,215),leaf(0,213)))))),node(22.0289,node(15.7873,node(3.41619,leaf(0,15),leaf(0,13)),node(11.7461,node(4.52272,leaf(0,20),leaf(0,12)),node(8.66891,leaf(0,17),node(6.28987,leaf(0,16),node(4.1643,leaf(0,14),node(2.63152,leaf(0,18),node(1.23779,leaf(0,19),leaf(0,11)))))))),node(18.9876,node(18.377,leaf(0,46),node(11.365,node(3.70601,leaf(0,55),node(2.0792,leaf(0,51),leaf(0,47))),node(11.2002,node(3.43336,leaf(0,49),leaf(0,48)),node(0.510546,node(0.265543,leaf(0,53),leaf(0,50)),node(0.256348,leaf(0,54),leaf(0,52)))))),node(15.6463,node(7.67865,leaf(0,128),node(6.62527,leaf(0,127),node(3.83734,leaf(0,130),leaf(0,129)))),node(12.6827,node(7.8782,node(3.27913,leaf(0,210),leaf(0,202)),node(3.30921,leaf(0,207),leaf(0,200))),node(9.81877,leaf(0,206),node(7.67803,node(4.38689,leaf(0,203),node(1.95379,leaf(0,208),leaf(0,205))),node(5.07992,leaf(0,204),node(3.03886,leaf(0,209),node(1.3832,leaf(0,211),leaf(0,201)))))))))))),node(32.417,node(19.6369,node(15.8446,leaf(0,178),node(6.41449,leaf(0,107),node(5.0674,leaf(0,111),node(1.95369,leaf(0,169),leaf(0,168))))),node(5.87586,node(4.01158,leaf(0,115),node(3.53901,node(3.51683,leaf(0,119),node(3.10084,leaf(0,117),leaf(0,112))),node(2.02926,leaf(0,121),node(0.881927,leaf(0,120),leaf(0,116))))),node(4.01897,leaf(0,122),node(1.80653,leaf(0,114),node(0.788242,leaf(0,118),leaf(0,113)))))),node(18.3368,node(13.6067,node(8.89218,node(8.8866,node(6.92967,node(6.80864,node(5.1224,leaf(0,227),node(3.73466,leaf(0,224),node(2.89694,leaf(0,226),node(0.794848,leaf(0,225),node(0.752118,leaf(0,231),node(0.69434,leaf(0,230),leaf(0,229))))))),leaf(0,217)),node(6.5521,node(3.80739,node(1.89428,leaf(0,184),node(1.58886,node(0.220763,leaf(0,179),leaf(0,181)),node(1.55073,node(0.527182,leaf(0,180),leaf(0,183)),node(0.532444,leaf(0,185),leaf(0,186))))),leaf(0,182)),node(5.19939,leaf(0,228),leaf(0,232)))),node(8.00142,leaf(0,198),node(3.87109,leaf(0,195),node(2.60969,leaf(0,199),node(1.03785,leaf(0,197),leaf(0,196)))))),node(3.48386,leaf(0,163),leaf(0,162))),node(11.9581,node(5.26084,node(3.49501,leaf(0,189),leaf(0,191)),node(4.31325,leaf(0,190),node(1.75508,leaf(0,188),leaf(0,187)))),node(6.03735,leaf(0,90),node(3.49057,leaf(0,87),node(0.805245,leaf(0,89),leaf(0,88)))))),node(11.2546,node(9.24085,node(6.71001,node(5.44218,node(1.96589,leaf(0,157),leaf(0,156)),node(2.3131,leaf(0,62),node(0.503238,leaf(0,60),leaf(0,61)))),node(2.22804,leaf(0,138),node(1.54275,leaf(0,223),leaf(0,170)))),node(7.49111,leaf(0,153),node(6.25647,node(3.58071,node(2.64826,node(0.135277,leaf(0,155),leaf(0,145)),node(1.65329,leaf(0,149),leaf(0,148))),node(2.24477,leaf(0,151),leaf(0,147))),node(4.43266,node(2.801,leaf(0,140),node(1.70814,node(0.716125,leaf(0,150),leaf(0,141)),node(1.05538,leaf(0,154),leaf(0,152)))),node(2.2296,leaf(0,143),node(0.912748,leaf(0,144),node(0.269011,leaf(0,146),leaf(0,142)))))))),node(8.28299,leaf(0,1),node(6.74964,leaf(0,161),node(5.7251,node(5.18071,leaf(0,92),leaf(0,86)),node(4.91227,node(0.841441,leaf(0,80),node(0.00861709,leaf(0,78),leaf(0,71))),node(4.21532,leaf(0,70),node(3.63333,node(2.55883,leaf(0,74),node(1.24026,node(0.786479,leaf(0,73),leaf(0,64)),node(0.778254,leaf(0,81),leaf(0,77)))),node(2.95357,node(2.32297,node(1.70858,node(1.0323,node(0.50272,leaf(0,69),leaf(0,79)),node(0.488649,leaf(0,65),leaf(0,63))),node(0.753017,leaf(0,76),leaf(0,68))),node(0.954775,leaf(0,75),leaf(0,72))),node(1.23446,leaf(0,67),leaf(0,66)))))))))))))))
    var str = JSON.stringify(tree, null, 32)
    console.log(str)
}

// convertTreeToJSONWebPPLFormat();

// Takes WebPPL formatted tree (currently hardcoded in local variable tree) and creates RootPPL JSON object (type and index omitted)
// (Which still need to be pre-processed). 
function convertTreeToJSONNoTypeNoIdx() {

    var leaf = function( a, i ) {
        return { age: a}
    }

    var node = function( a, l, r ) {
        return { age: a, left: l, right: r }
    }

    var tree = node(13.015999999999613,node(10.625999999999765,node(8.351999999999904,node(7.6789999999999035,node(5.187000000000067,leaf(0,1),leaf(0,2)),node(5.19600000000007,leaf(0,3),node(4.870999999999686,node(2.6009999999998246,leaf(0,4),leaf(0,5)),leaf(0,6)))),node(7.360999999999658,node(3.8179999999998175,node(1.143000000000001,node(0.8290000000000006,leaf(0,7),leaf(0,8)),leaf(0,9)),node(1.8129999999999613,node(0.45200000000000035,node(0.20300000000000015,leaf(0,10),leaf(0,11)),leaf(0,12)),leaf(0,13))),node(1.8680000000000012,node(0.8660000000000007,leaf(0,14),node(0.001,leaf(0,15),leaf(0,16))),node(1.059999999999994,leaf(0,17),leaf(0,18))))),node(10.53600000000035,node(8.291000000000485,node(1.3959999999999808,node(0.21500000000000016,leaf(0,19),leaf(0,20)),leaf(0,21)),leaf(0,22)),node(8.192000000000883,node(0.5600000000000004,leaf(0,23),leaf(0,24)),leaf(0,25)))),node(8.957999999999615,node(3.7479999999996982,leaf(0,26),node(0.03300000000000002,leaf(0,27),leaf(0,28))),node(7.775000000000737,node(0.5840000000000004,leaf(0,29),leaf(0,30)),node(1.5889999999999358,leaf(0,31),leaf(0,32)))))
    
    // console.log(tree);

    var str = JSON.stringify(tree, null, 32)
    console.log(str)
}

// convertTreeToJSONNoTypeNoIdx();
