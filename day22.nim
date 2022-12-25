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
  Direction3D = enum normX, normY, normZ, normNX, normNY, normNZ
  Face = object
    coord                  : Coord
    prev_coord             : Coord
    normal                 : Direction3D
    updir, rightdir        : Direction3D
  CubeFaces = array[Direction3D, Face] # keyed by normal

import deques

proc rotateX90(dir: Direction3D): Direction3D =
  case dir
  of normX: normX
  of normY: normZ
  of normZ: normNY
  of normNX: normNX
  of normNY: normNZ
  of normNZ: normY
proc rotateNX90(dir: Direction3D): Direction3D =
  dir.rotateX90.rotateX90.rotateX90
proc rotateY90(dir: Direction3D): Direction3D =
  case dir
  of normX: normNZ
  of normY: normY
  of normZ: normX
  of normNX: normZ
  of normNY: normNY
  of normNZ: normNX
proc rotateNY90(dir: Direction3D): Direction3D =
  dir.rotateY90.rotateY90.rotateY90

proc `-`(dir: Direction3D): Direction3D =
  case dir
  of normX: normNX
  of normY: normNY
  of normZ: normNZ
  of normNX: normX
  of normNY: normY
  of normNZ: normZ

proc rotate90Along(dir, axis: Direction3D): Direction3D =
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
proc traceNet(m: Map; start: Coord; face_len: int = 50): tuple[faces: CubeFaces, prevs: Table[Coord, Coord]] =
  result.prevs = initTable[Coord, Coord]()
  #result.faces = @[]
  var q = Deque[Face]()
  q.addLast Face(coord: start, normal: normNZ, updir: normNY, rightdir: normX)
  result.prevs[q.peekFirst.coord] = q.peekFirst.coord
  result.faces[normNZ] = q.peekFirst
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
        result.faces[new_face.normal] = new_face
        result.prevs[new_face.coord] = cur.coord
        q.addLast new_face


func to3D(dir: Heading; up, right: Direction3D): Direction3D =
  case dir
  of East: right
  of South: -up
  of West: -right
  of North: up

func to2D(dir: Direction3D; up, right: Direction3D): Heading =
  if dir == up:
    North
  elif dir == -up:
    South
  elif dir == right:
    East
  #elif dir == -right:
  else:
    West

proc transitionFace(m: Map; faces: CubeFaces; cur_face: Direction3D; row, col: int; dir: Heading; face_size: Natural):
    tuple[row: int, col: int, dir: Heading] =
  let face = faces[cur_face]
  let (rotate_axis, next_normal, edge_coord) = (case dir
    of North:
      let rotate_axis = -face.rightdir
      let next_face_norm = cur_face.rotate90Along(rotate_axis)
      var coord = col-face.coord.col
      # if not(N edge of cur == S edge of next or N edge of cur == W edge of next)
      if not(faces[next_face_norm].rightdir == face.rightdir or -faces[next_face_norm].updir == face.rightdir):
        coord = (face_size - 1) - coord
      (rotate_axis, next_face_norm, coord)
    of East:
      let rotate_axis = face.updir
      let next_face_norm = cur_face.rotate90Along(rotate_axis)
      var coord = row - face.coord.row
      # if not(E edge of cur == W edge of next or E edge of cur == S edge of next)
      if not(faces[next_face_norm].updir == face.updir or -faces[next_face_norm].rightdir == face.updir):
        coord = (face_size - 1) - coord
      (face.updir, next_face_norm, coord)
    of South:
      let rotate_axis = face.rightdir
      let next_face_norm = cur_face.rotate90Along(rotate_axis)
      var coord = col - face.coord.col
      # if not(S edge of cur == N edge of next or S edge of cur == E edge of next)
      if not(face.rightdir == faces[next_face_norm].rightdir or face.right_dir == -faces[next_face_norm].updir):
        coord = (face_size - 1) - coord
      (rotate_axis, next_face_norm, coord)
    of West:
      let rotate_axis = -face.updir
      let next_face_norm = cur_face.rotate90along(rotate_axis)
      var coord = row - face.coord.row
      # if not(W edge of cur == E edge of next or W edge of cur == N edge of next
      if not(face.updir == faces[next_face_norm].updir or face.updir == -faces[next_face_norm].rightdir):
        coord = (face_size - 1) - coord
      (rotate_axis, next_face_norm, coord))

  let next_face = faces[next_normal]
  result.dir = dir.to3D(face.updir, face.rightdir)
                  .rotate90Along(rotate_axis)
                  .to2D(next_face.updir, next_face.rightdir)
  # negative edge coordinate indicates that it needs to be inverted
  case result.dir
  of North: # we're heading north, so we're on the south edge
    result.row = next_face.coord.row + face_size-1
    result.col = next_face.coord.col + edge_coord
  of East: # we're on the W edge
    result.row = next_face.coord.row + edge_coord
    result.col = next_face.coord.col
  of South: # we're on the N edge
    result.row = next_face.coord.row
    result.col = next_face.coord.col + edge_coord
  of West: # we're on the E edge
    result.row = next_face.coord.row + edge_coord
    result.col = next_face.coord.col + face_size-1


#proc transitionFace(m: Map; faces: CubeFaces; cur_face: Direction3D; row, col: int; dir: Heading; face_size: Natural):
#    tuple[row: int, col: int, dir: Heading] =


func determineFace(faces: CubeFaces; row, col: int; face_size: Natural): Direction3D =
  for dir, face in faces:
    if row in face.coord.row .. face.coord.row + face_size-1 and
        col in face.coord.col .. face.coord.col + face_size-1:
      return dir
  assert(false, "Coordinate not found in face: " & $(row: row, col: col))

proc advance2(m: Map; faces: CubeFaces; row, col: int; dir: Heading;
              face_size: Natural): tuple[row: int, col: int, dir: Heading] =
  case dir
  of East:
    var (row, col) = (row, col+1)
    var dir = dir
    if col >= m[row].len or m[row][col] == ' ':
      dec col
      let cur_face = determineFace(faces, row, col, face_size)
      (row, col, dir) = m.transitionFace(faces, cur_face, row, col, dir, face_size)
    result = (row, col, dir)
  of South:
    var (row, col) = (row+1, col)
    var dir = dir
    if row >= m.len or m[row][col] == ' ':
      dec row
      let cur_face = determineFace(faces, row, col, face_size)
      (row, col, dir) = m.transitionFace(faces, cur_face, row, col, dir, face_size)
    result = (row, col, dir)
  of West:
    var (row, col) = (row, col-1)
    var dir = dir
    if col < 0 or m[row][col] == ' ':
      inc col
      let cur_face = determineFace(faces, row, col, face_size)
      (row, col, dir) = m.transitionFace(faces, cur_face, row, col, dir, face_size)
    result = (row, col, dir)
  of North:
    var (row, col) = (row-1, col)
    var dir = dir
    if row < 0 or m[row][col] == ' ':
      inc row
      let cur_face = determineFace(faces, row, col, face_size)
      (row, col, dir) = m.transitionFace(faces, cur_face, row, col, dir, face_size)
    result = (row, col, dir)
  if m[result.row][result.col] == '#':
    return (row, col, dir)
  else:
    return result


proc doRounds2(m: Map; row, col: int; dir: Heading; cmds: seq[Command],
               faces: CubeFaces; face_size: Natural = 50): tuple[row: int, col: int, dir: Heading] =
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
        let new_rc = advance2(m, faces, result.row, result.col, result.dir, face_size)
        result.row = new_rc.row
        result.col = new_rc.col
        result.dir = new_rc.dir
        dec cmds[idx].steps
    of ckRotate:
      result.dir = ROTATE_DIRS[result.dir][cmds[idx].rot_dir]
      inc idx

# proc transitionFace(m: Map; faces: CubeFaces; cur_face: Direction3D; row, col: int; dir: Heading; face_size: Natural):

when isMainModule:
  proc main =
    let (m, cmds) = parseInput(paramStr(1))
    let (row, col) = m.getStartingPosition
    let (final_row, final_col, final_dir) = doRounds(m, row, col, East, cmds)
    echo "Final row: ", final_row+1
    echo "Final col: ", final_col+1
    echo "Final dir: ", final_dir
    echo "Part 1 result: ", 1000 * (1+final_row) + 4 * (1+final_col) + final_dir.ord

    let (faces, prevs) = traceNet(m, (row, col), 50)
    for norm, face in faces:
      echo norm, ": ", face
    echo ""

    let (final_row2, final_col2, final_dir2) = doRounds2(m, row, col, East, cmds, faces, 50)
    echo final_row2, ", ", final_col2
    echo final_dir2
    echo "Part 2 result: ", 1000 * (1+final_row2) + 4 * (1+final_col2) + final_dir2.ord

  main()
