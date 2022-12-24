from math import lcm

from algorithm import reverse
import deques
import intsets
import sequtils
import strutils
import tables


type
  # as with Day 23, taking advantage of Nim's IntSets by packing two
  # coordinates as int32's into an int64, and storing the int64 in the intset
  Coord = tuple[row: int32, col: int32]
  CoordSet = object
    data  : IntSet
    min_coord, max_coord: Coord

  Direction = enum N, E, S, W
  CoordAndDir = tuple[c: Coord, dir: Direction]

func toInt(c: Coord): int =
  cast[int](c)
func toCoord(x: int): Coord =
  cast[Coord](x)

func initCoordSet(min_coord, max_coord: Coord): CoordSet =
  result.data = initIntSet()
  result.min_coord = min_coord
  result.max_coord = max_coord

proc incl(self: var CoordSet; c: Coord) =
  self.data.incl c.toInt

func contains(self: CoordSet; c: Coord): bool =
  c.toInt in self.data

# The blizzard configuration is ultimately periodic. All blizzards traveling horizontally
# will end up at their original positions after map-width rounds, and all vertical blizzards
# after map-height rounds. Thus, the whole map is periodic after lcm(map-width, map-height)
# rounds. This periodicity can in theory be used to reduce the state space in the subsequent
# BFS. In practice (for my input, at least), the path even in Part 2 was shorter than the
# period, meaning that the algorithm never reached the point where it could take advantage of
# the reduction in state space.
#
# Generates all lcm(map-width, map-height) configurations of the blizzard. The map for a given
# round is at the `round mod maps.len`'th index.
proc genMaps(coords: openarray[CoordAndDir]; min_coord, max_coord: Coord): seq[CoordSet] =
  result = @[]
  let width = max_coord.col - min_coord.col + 1
  let height = max_coord.row - min_coord.row + 1
  echo "Width: ", width, ", ", "Height: ", height
  let cyc_len = lcm(width, height)
  echo "Period: ", cyc_len
  for round in 0 ..< cyc_len:
    var round_coords = initCoordSet(min_coord, max_coord)
    # add blizzard coords to this round's map
    for c, dir in coords.items:
      var (new_row, new_col) = c
      case dir
      of N:
        # why is signed modulus the default behavior in C? I NEVER NEED IT
        # adding a sufficiently large multiple (cyc_len) of the divisor to the dividend to make sure
        # that the dividend is not negative
        new_row = ((new_row - round - min_coord.row + cyc_len*height) mod height) + min_coord.row
      of E:
        new_col = ((new_col + round - min_coord.col + cyc_len*width) mod width) + min_coord.col
      of S:
        new_row = ((new_row + round - min_coord.row + cyc_len*height) mod height) + min_coord.row
      of W:
        new_col = ((new_col - round - min_coord.col + cyc_len*width) mod width) + min_coord.col
      round_coords.incl ((row: new_row, col: new_col))
    result.add round_coords

proc parseInput(fname: string): tuple[maps: seq[CoordSet]; start: Coord; stop: Coord] =
  var coords = newSeq[CoordAndDir]()
  var max_coord : Coord
  for ln in fname.lines:
    max_coord = (row: max_coord.row, col: ln.high.int32)
    for col, c in ln.strip:
      if c in {'^', '>', 'v', '<'}:
        coords.add (
          c: (row: max_coord.row, col: col.int32),
          dir: (case c
            of '^': N
            of '>': E
            of 'v': S
            else: W
          )
        )
    inc max_coord.row
  dec max_coord.row

  # off-by-one city
  let field_min : Coord = (row: 1.int32, col: 1.int32)
  let field_max : Coord = (row: max_coord.row-1, col: max_coord.col-1)
  result.start = (row: 0.int32, col: 1.int32)
  result.stop = (row: max_coord.row, col: max_coord.col-1)
  result.maps = genMaps(coords, field_min, field_max)


# stringify for debugging
proc `$`(m: CoordSet): string =
  result = ""
  for row in m.min_coord.row .. m.max_coord.row:
    for col in m.min_coord.col .. m.max_coord.col:
      if (row: row, col: col) in m:
        result &= '#'
      else:
        result &= '.'
    result &= "\n"


func getNeighbors(c: Coord): array[0..4, Coord] =
  # can stay put
  [c,
   # or move along cardinal directions
   (row: c.row-1, col: c.col),
   (row: c.row, col: c.col+1),
   (row: c.row+1, col: c.col),
   (row: c.row, col: c.col-1)]

type
  # coords are represented as ints here
  SearchState = tuple[coord: int, round: Natural]

# Traverse the blizzard from start to stop starting with the blizzard configuration of the
# start_round'th round (breadth-first search). Returns the final search state and the shortest-
# path list for all states encountered (used for retracing the path)
proc BFS(maps: openarray[CoordSet]; start, stop: Coord; start_round: Natural = 0):
    tuple[final: SearchState,
          prevs: Table[SearchState, SearchState]] =

  let start_state : SearchState = (coord: start.toInt, round: start_round)
  var q = initDeque[SearchState]()
  q.addLast start_state

  # dummy initialization of the starting state so that it is identified as "seen" later
  result.prevs = initTable[SearchState, SearchState]()
  result.prevs[start_state] = start_state

  while true:
    let (prev_pos, prev_round) = q.popFirst
    # if we've reached our destination coordinate, we're done
    if prev_pos.toCoord == stop:
      result.final = (prev_pos, prev_round)
      return result

    # advance the round counter
    let cur_round = ((prev_round+1) mod maps.len).Natural
    # contains blizzard positions
    let cur_map = maps[cur_round]

    for neighbor in prev_pos.toCoord.getNeighbors:
      # if there's not a blizzard at neighbor and we're in bounds, we can move there
      if ((neighbor.row in cur_map.min_coord.row .. cur_map.max_coord.row and
           neighbor.col in cur_map.min_coord.col .. cur_map.max_coord.col) or neighbor == start or neighbor == stop) and
          neighbor notin cur_map:
        let new_state : SearchState = (coord: neighbor.toInt, round: cur_round)
        # if we haven't seen this configuration (state) yet
        if new_state notin result.prevs:
          result.prevs[new_state] = (prev_pos, prev_round)
          q.addLast new_state

# Trace the path from start to stop using the shortest-path information contained in `prevs`
# The final path (a list of `Coord`s) does not contain `start`
func tracePath(prevs: Table[SearchState, SearchState]; start, stop: SearchState): seq[Coord] =
  result = @[]
  var cursor = stop
  while cursor != start:
    result.add cursor.coord.toCoord
    cursor = prevs[cursor]
  result.reverse

when isMainModule:
  import os
  proc main =
    let (maps, start, stop) = parseInput(paramStr(1))
    let
      (final, prevs) = BFS(maps, start, stop)
      start_state : SearchState = (start.toInt, 0.Natural)
      path = tracePath(prevs, start_state, final)
    echo "Length of first leg (Part 1): ", path.len

    # We can greedily run the next BFS using the ending state of the first as the starting point.
    # i.e., len(a->a' via coord b) == len(a->b) + len(b->a'), where a and b labels indicate a
    # specific coordinate, different ticks represent different time points, and x->y is the
    # shortest path from x to y
    #
    # One might suppose that there is a path a->a' via b' where len(a->b') > len(a->b) and
    # len(a->b'->a') < len(a->b->a') (i.e., that there is an alternate first leg that
    # is longer than the shortest-path first leg, that allows for a shorter second leg and
    # ultimately a shorter total path). As a corollary, len(b->a') > len(b'->a')
    # However, this is impoossible; because b and b' correspond
    # to the same location (with b' respresenting a later time), there is always a path b->b' such
    # that len(a->b->b') == len(a->b'), corresponding to staying put at the coordinate represented
    # by b until the time of b'. Thus, a->b is always a possible subpath of a->b', and thus of
    # a->b'->a'. In addition, because, b->a' is a shortest path, then b->b'->a' is also the shortest
    # path. Thus len(a->b->a') == len(a->b->b'->a'), contradicting our assertion that len(a->b'->a') <
    # len(a->b->a').
    let
      # start the search for the next leg at the (periodic) round counter
      (final2, prevs2) = BFS(maps, stop, start, final.round)
      start_state_2 : SearchState = (stop.toInt, final.round)
      path2 = tracePath(prevs2, start_state_2, final2)
    echo "Length of second leg: ", path2.len

    let
      (final3, prevs3) = BFS(maps, start, stop, final2.round)
      start_state_3 : SearchState = (start.toInt, final2.round)
      path3 = tracePath(prevs3, start_state_3, final3)
    echo "Length of third leg: ", path3.len

    echo "Total path length (Part 2): ", path.len + path2.len + path3.len

  main()
