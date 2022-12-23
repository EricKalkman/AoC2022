import os
import strutils
import sequtils

type
  Heading = enum East, South, West, North
  RotateDirection = enum rdCW, rdCCW
  CommandKind = enum ckMove, ckRotate
  Command = object
    case kind: CommandKind
    of ckMove:
      steps: Natural
    of ckRotate:
      rot_dir: RotateDirection

  Map = seq[string]

  Coord = tuple
    row: int
    col: int

proc `$`(m: Map): string =
  result = ""
  for ln in m:
    result &= ln & "\n"

proc parseMap(map_lines: openarray[string]): Map =
  var mx = 0
  for ln in map_lines:
    if len(ln) > mx:
      mx = ln.len
  result = @[]
  for ln in map_lines:
    result.add(ln & repeat(' ', mx-len(ln)))

proc parseCommands(cmd_line: string): seq[Command] =
  var idx = 0
  while idx < len(cmd_line):
    if cmd_line[idx] == 'L':
      result.add Command(kind: ckRotate, rot_dir: rdCCW)
      inc idx
    elif cmd_line[idx] == 'R':
      result.add Command(kind: ckRotate, rot_dir: rdCW)
      inc idx
    else:
      let start = idx
      while idx < cmd_line.len and cmd_line[idx] in DIGITS:
        inc idx
      result.add Command(kind: ckMove, steps: cmd_line[start .. idx-1].parseInt)

proc parseInput(fname: string): tuple[m: Map, cmds: seq[Command]] =
  let inp_lines = toSeq(fname.lines)
  var idx = 0
  while inp_lines[idx] != "":
    inc idx
  result.m = parseMap(inp_lines[0 .. idx-1])
  result.cmds = parseCommands(inp_lines[idx+1])

proc advance(m: Map; row, col: int; dir: Heading): tuple[row: int, col: int] =
  case dir
  of East:
    var (row, col) = (row, col+1)
    if col >= m[row].len or m[row][col] == ' ':
      dec col
      while col >= 0 and m[row][col] != ' ':
        dec col
      inc col
    result = (row, col)
  of South:
    var (row, col) = (row+1, col)
    if row >= m.len or m[row][col] == ' ':
      dec row
      while row >= 0 and m[row][col] != ' ':
        dec row
      inc row
    result = (row, col)
  of West:
    var (row, col) = (row, col-1)
    if col < 0 or m[row][col] == ' ':
      inc col
      while col < m[row].len and m[row][col] != ' ':
        inc col
      dec col
    result = (row, col)
  of North:
    var (row, col) = (row-1, col)
    if row < 0 or m[row][col] == ' ':
      inc row
      while row < m.len and m[row][col] != ' ':
        inc row
      dec row
    result = (row, col)
  if m[result.row][result.col] == '#':
    return (row, col)
  else:
    return result

proc doRounds(m: Map; row, col: int; dir: Heading; cmds: seq[Command]): tuple[row: int, col: int, dir: Heading] =
  const ROTATE_DIRS: array[Heading, array[RotateDirection, Heading]] = [
    East: [rdCW: South, rdCCW: North],
    South: [rdCW: West, rdCCW: East],
    West: [rdCW: North, rdCCW: South],
    North: [rdCW: East, rdCCW: West]
  ]
  result = (row, col, dir)
  var cmds = cmds
  var idx = 0
  while idx < cmds.len:
    case cmds[idx].kind
    of ckMove:
      if cmds[idx].steps == 0:
        inc idx
      else:
        let new_rc = advance(m, result.row, result.col, result.dir)
        result.row = new_rc.row
        result.col = new_rc.col
        dec cmds[idx].steps
    of ckRotate:
      result.dir = ROTATE_DIRS[result.dir][cmds[idx].rot_dir]
      inc idx

proc getStartingPosition(m: Map): tuple[row: int, col: int] =
  while m[0][result.col] != '.': inc result.col

import tables

type
  FacePlane = enum xy, xz, yz
  FaceNormal = enum normX, normY, normZ, normNX, normNY, normNZ
  Face = object
    coord                  : Coord
    prev_coord             : Coord
    normal                 : FaceNormal
    updir, rightdir        : FaceNormal

import deques

proc rotateX90(dir: FaceNormal): FaceNormal =
  case dir
  of normX: normX
  of normY: normZ
  of normZ: normNY
  of normNX: normNX
  of normNY: normNZ
  of normNZ: normY
proc rotateNX90(dir: FaceNormal): FaceNormal =
  dir.rotateX90.rotateX90.rotateX90
proc rotateY90(dir: FaceNormal): FaceNormal =
  case dir
  of normX: normNZ
  of normY: normY
  of normZ: normX
  of normNX: normZ
  of normNY: normNY
  of normNZ: normNX
proc rotateNY90(dir: FaceNormal): FaceNormal =
  dir.rotateY90.rotateY90.rotateY90

proc `-`(dir: FaceNormal): FaceNormal =
  case dir
  of normX: normNX
  of normY: normNY
  of normZ: normNZ
  of normNX: normNX
  of normNY: normNY
  of normNZ: normNZ

proc rotate90Along(dir, axis: FaceNormal): FaceNormal =
  case dir
  of normX:
    case axis
    of normY: normNZ
    of normZ: normY
    of normNY: normZ
    of normNZ: normNY
    else: dir
  of normY:
    case axis
    of normX: normZ
    of normZ: normNX
    of normNX: normNZ
    of normNZ: normX
    else: dir
  of normZ:
    case axis
    of normX: normNY
    of normY: normX
    of normNX: normY
    of normNY: normNX
    else: dir
  of normNX:
    case axis
    of normY: normZ
    of normZ: normNY
    of normNY: normNZ
    of normNZ: normY
    else: dir
  of normNY:
    case axis
    of normX: normNZ
    of normZ: normX
    of normNX: normZ
    of normNZ: normNX
    else: dir
  of normNZ:
    case axis
    of normX: normY
    of normY: normNX
    of normNX: normNY
    of normNY: normX
    else: dir


# BFS trace of faces
proc traceNet(m: Map; start: Coord; face_len: int = 50): tuple[faces: seq[Face], prevs: Table[Coord, Coord]] =
  result.prevs = initTable[Coord, Coord]()
  result.faces = @[]
  var q = Deque[Face]()
  q.addLast Face(coord: start, normal: normNZ, updir: normNY, rightdir: normX)
  result.prevs[q.peekFirst.coord] = q.peekFirst.coord
  result.faces.add q.peekFirst
  while q.len > 0:
    let cur = q.popFirst
    let (row, col) = cur.coord
    let norm = cur.normal
    let updir = cur.updir
    let rdir = cur.rightdir
    for neighbor_coord, nnorm, nupdir, nrdir in
        [((row: row+face_len, col: col), rotate90Along(norm, rdir), rotate90Along(updir, rdir), rdir),
         ((row: row-face_len, col: col), rotate90Along(norm, -rdir), rotate90Along(updir, -rdir), rdir),
         ((row: row, col: col+face_len), rotate90Along(norm, updir), updir, rotate90Along(rdir, updir)),
         ((row: row, col: col-face_len), rotate90Along(norm, -updir), updir, rotate90along(rdir, -updir))].items:
      if neighbor_coord.row in 0 .. m.high and neighbor_coord.col in 0 .. m[0].high and
          not result.prevs.hasKey(neighbor_coord) and m[neighbor_coord.row][neighbor_coord.col] != ' ':
        let new_face = Face(coord: neighbor_coord,
                            prev_coord: cur.coord,
                            normal: nnorm,
                            updir: nupdir,
                            rightdir: nrdir)
        result.faces.add new_face
        result.prevs[new_face.coord] = cur.coord
        q.addLast new_face


when isMainModule:
  proc main =
    let (m, cmds) = parseInput(paramStr(1))
    let (row, col) = m.getStartingPosition
    let (final_row, final_col, final_dir) = doRounds(m, row, col, East, cmds)
    echo "Final row: ", final_row+1
    echo "Final col: ", final_col+1
    echo "Final dir: ", final_dir
    echo "Part 1 result: ", 1000 * (1+final_row) + 4 * (1+final_col) + final_dir.ord

    let (faces, prevs) = traceNet(m, (row, col))
    for face in faces:
      echo face


  main()
