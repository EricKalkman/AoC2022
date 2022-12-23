import intsets
import os
import strutils
import sequtils
import tables

# A special note about formats: this program uses sets and hash tables to
# keep track of the positions of each elf. Coordinates are represented as
# int32 tuples, which can be converted into 64-bit ints for compatibility
# with the intsets module

type
  Tile = enum tEmpty, tElf
  Coord = tuple[row: int32, col: int32]
  Direction = enum NW, N, NE, W, E, SW, S, SE

# convert coordinate to 64-bit int representation
proc toInt(coord: Coord): int =
  cast[int](coord)
# convert 64-bit representation back to coordinate
proc toCoord(val: int): Coord =
  cast[Coord](val)

proc parseInput(fname: string): IntSet =
  result = initIntSet()
  var i = 0
  for ln in fname.lines:
    for j, c in ln.strip:
      if c == '#':
        result.incl toInt((row: i.int32, col: j.int32))
    inc i

# returns all 8 cardinal and diagonal neighbors of a given coordinate
# could be made more efficient by explicitly typing out the different directions
func getNeighbors(coord: Coord): array[Direction, Coord] =
  var idx = 0
  for row in coord.row-1 .. coord.row+1:
    for col in coord.col-1 .. coord.col+1:
      if (row, col) != coord:
        result[idx.Direction] = (row, col)
        inc idx

proc doRound(elfs: var IntSet; round: Natural) =
  ## LOOKUP TABLES
  # table representing the neighbors that we need to check in order to move
  # in that direction. Only matters for cardinal directions; non-cardinal
  # directions are dummy values. This could be improved by reordering the
  # Direction enum, but that makes getNeighbors more tedious to write
  const GENERAL_DIRS : array[Direction, array[0..2, Direction]] = [
    NW: [N, N, N], N: [NW, N, NE], NE: [N, N, N],
    W:  [NW, W, SW],               E: [NE, E, SE],
    SW: [N, N, N], S: [SW, S, SE], SE: [N, N, N]
  ]
  # Order of the directions to consider for a given round. The round #
  # is taken mod 4 and used as the key for this table
  const DIR_SELECT : array[0..3, array[0..3, Direction]] = [
    [N, S, W, E], [S, W, E, N], [W, E, N, S], [E, N, S, W]
  ]

  # map of proposed moves to the positions of the elfs that proposed them
  # uses the 64-bit int representation for coordinates
  var prev_map = initTable[int, seq[int]]()
  for elf in elfs:
    let neighbors = elf.toCoord.getNeighbors
    # if there are no neighbors, don't move
    if not anyIt(Direction, neighbors[it].toInt in elfs):
      continue
    # try moving N, S, W, or E (in whatever is prescribed for the round)
    for check_dir in DIR_SELECT[round mod 4]:
      # if there is no neighbor elf in the current trial direction
      if not anyIt(GENERAL_DIRS[check_dir], neighbors[it].toInt in elfs):
        # keep track of the proposal
        prev_map.mgetOrPut(neighbors[check_dir].toInt, @[]).add elf
        break

  for prop_pos, proposing_elfs in prev_map.pairs:
    if proposing_elfs.len == 1:
      assert proposing_elfs[0] in elfs
      #echo proposing_elfs[0].toCoord.row, ", ", proposing_elfs[0].toCoord.col, " -> ",
      #  prop_pos.toCoord.row, ", ", prop_pos.toCoord.col
      elfs.excl proposing_elfs[0]
      elfs.incl prop_pos

# get the rectangular box around the elfs
func getBoundingBox(elfs: IntSet): tuple[minrow: int32, mincol: int32, maxrow: int32, maxcol: int32] =
  let coords : seq[Coord] = toSeq(elfs).mapIt(it.toCoord)
  result.minrow = coords.foldl(if a.row < b.row: a else: b).row
  result.maxrow = coords.foldl(if a.row > b.row: a else: b).row
  result.mincol = coords.foldl(if a.col < b.col: a else: b).col
  result.maxcol = coords.foldl(if a.col > b.col: a else: b).col

# strinig representation of the map (for debugging)
func `$`(elfs: IntSet): string =
  result = ""
  let (minrow, mincol, maxrow, maxcol) = getBoundingBox(elfs)
  for row in minrow .. maxrow:
    for col in mincol .. maxcol:
      if (row, col).toInt in elfs:
        result &= '#'
      else:
        result &= '.'
    result &= "\n"

# returns the answer for Part 1 when given the final positions of the elfs
func numOpenTiles(elfs: IntSet): Natural =
  let
    (minrow, mincol, maxrow, maxcol) = getBoundingBox(elfs)
    area = (maxrow-minrow+1)*(maxcol-mincol+1)
  return area - elfs.len


when isMainModule:
  proc main =
    var elf_coords = parseInput(paramStr(1))
    echo elf_coords.getBoundingBox
    echo "Starting board:"
    echo elf_coords
    for round in 0 .. 9:
      elf_coords.doRound(round)
    echo "Board after 10 rounds:"
    echo elf_coords
    echo "Part 1: ", elf_coords.numOpenTiles, "\n"

    var round = 0
    elf_coords = parseInput(paramStr(1))
    while true:
      # keep track of the current state
      var old_coords : IntSet
      old_coords.assign(elf_coords)
      # update positions
      elf_coords.doRound(round)
      inc round
      # if no positions changed, then we're done
      if elf_coords == old_coords:
        break

    echo "Final board:"
    echo elf_coords
    echo "First round with no movement: ", round

  main()
