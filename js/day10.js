
function rope(size) {
  const r = [];
  for(let i = 0; i < size; i++) {
    r.push(i);
  }

  return r;
}

function twist(rope, start, len) {
  const end = start + len;
  let slice;
  if (end > rope.length) {
    const head = rope.slice(start);
    const tail = rope.slice(0, end % rope.length);
    slice = head.concat(tail); 

  } else {
    slice = rope.slice(start, end);
  }

  return splice(rope, slice.reverse(), start);
}


function splice(rope, slice, start) {
  slice.forEach((n,i) => {
    rope[(start + i) % rope.length] = n;
  });

  return rope;
}

function fail(msg, v) {
  console.error(msg, v);
  throw msg;
}

function assertEquals(a,b) {
  if (a !== b) fail("not=",a,b);
}

function assertArrayEquals(a, b) {
  if (a.length !== b.length) {
    fail("arrays not=", [a,b]);
  }

  for(let i in a) {
    if(a[i] !== b[i]) {
      fail("arrays not=", [a,b]);
    }
  }
}

function knotHash(size, lengths) {
  let r = rope(size),
    skip = 0, 
    pos = 0;

  lengths.forEach((len) => {
    r = twist(r, pos, len);
    pos = (pos + len + skip) % size;
    skip++;
  });

  return r;
}

function solvePart1() {
  const lens = [212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164];
  const r = knotHash(256, lens);
  return r[0] * r[1];
}

function test() {
  // test non wrapping
  assertArrayEquals([0,1,3,2,4,5,6,7,8,9], twist(rope(10), 2, 2));

  // test wrapping
  assertArrayEquals([3, 1, 2, 0, 4], twist(rope(5), 3, 3));

  // test wrapping and reverse whole list
  assertArrayEquals([3,4,2,1,0], twist([4,3,0,1,2], 1, 5));

  // test example
  assertArrayEquals([3, 4, 2, 1, 0], knotHash(5, [3,4,1,5]));

  // test solution to part1
  assertEquals(212, solvePart1());

  console.log("ALL TESTS PASSED!");
}

test();




