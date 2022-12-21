import strutils

type
  DLL = ref object
    prev, next: DLL
    val     : int
    orig_idx: int

proc loadDLL(fname: string): tuple[orig: seq[int], d: DLL] =
  new result.d
  let first = result.d
  var i = 0
  for ln in fname.lines:
    result.orig.add ln.strip.parseInt
    result.d.val = result.orig[^1]
    result.d.orig_idx = i
    var new_node = new(DLL)
    new_node.prev = result.d
    result.d.next = new_node
    result.d = new_node
    inc i
  result.d = result.d.prev
  result.d.next = first
  first.prev = result.d

proc fromSeq(arr: openarray[int]): DLL =
  new result
  let first = result
  for i, x in arr:
    result.val = x
    result.orig_idx = i
    let new_node = new(DLL)
    new_node.prev = result
    result.next = new_node
    result = new_node
  result = result.prev
  result.next = first
  first.prev = result

proc `$`(d: DLL): string =
  result = ""
  var walker = d
  while true:
    result &= $walker.val & ", "
    walker = walker.next
    if walker == d:
      break

proc mix(d: DLL; orig: openarray[int]) =
  var walker = d
  for orig_idx, val in orig:
    while not (walker.val == val and walker.orig_idx == orig_idx):
      walker = walker.next
    let n_shifts = val.abs mod (orig.len-1)
    for i in 1 .. n_shifts:
      if val > 0:
        swap walker.val, walker.next.val
        swap walker.orig_idx, walker.next.orig_idx
        walker = walker.next
      else:
        swap walker.val, walker.prev.val
        swap walker.orig_idx, walker.prev.orig_idx
        walker = walker.prev


proc answer(d: DLL): int =
  var walker = d
  while walker.val != 0:
    walker = walker.next
  for i in 1 .. 3:
    for j in 1 .. 1000:
      walker = walker.next
    result += walker.val

const KEY = 811589153

when isMainModule:
  proc main =
    var (orig, d) = loadDLL("inputs/day20.inp")
    d.mix(orig)
    echo "Part 1: ", d.answer
    for val in orig.mitems:
      val *= KEY
    d = fromSeq(orig)
    for i in 1 .. 10:
      d.mix(orig)
    echo "Part 2: ", d.answer

  main()
