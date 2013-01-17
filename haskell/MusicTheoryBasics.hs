--
-- Haskell representation of some basic music theory of scales & chords
-- 
-- Remko Tronçon
-- http://el-tramo.be
--

import List (sort, sortBy, findIndex, intersect)

--------------------------------------------------------------------------------
-- The Theory
--------------------------------------------------------------------------------

--
-- Intervals of all scales.
-- All intervals are a rotation of each other.
--
intervals :: ScaleName -> [Interval]
intervals Ionian = [2,2,1,2,2,2,1]
intervals Major = intervals Ionian
intervals Minor = intervals Aeolian
intervals scale = shift (intervals Ionian) (rank scale) 
  where
    order = [Dorian,Phrygian,Lydian,Myxolidian,Aeolian,Locrian]
    rank scale = case (findIndex (scale ==) order) of Just i -> i+1
    shift l n = case (splitAt n l) of (a,b) -> b ++ a

--
-- The notes making up the chords
--
chordNotes Five = [(ScaleNote Major 1), (ScaleNote Major 5)]
chordNotes Maj = (chordNotes Five) ++ [(ScaleNote Major 3)]
chordNotes Min = (chordNotes Five) ++ [(ScaleNote Minor 3)]
chordNotes Maj7 = (chordNotes Maj) ++ [(ScaleNote Major 7)]
chordNotes Min7 = (chordNotes Min) ++ [(ScaleNote Minor 7)]
chordNotes Dom7 = (chordNotes Maj) ++ [(ScaleNote Minor 7)]
chordNotes Maj6 = (chordNotes Maj) ++ [(ScaleNote Major 6)]
chordNotes Min6 = (chordNotes Min) ++ [(ScaleNote Major 6)]
chordNotes Sus4 = (chordNotes Five) ++ [(ScaleNote Major 4)]
chordNotes Sus2 = (chordNotes Five) ++ [(ScaleNote Major 2)]
chordNotes MajorAdd2 = (chordNotes Maj) ++ [(ScaleNote Major 2)]
chordNotes MinorAdd2 = (chordNotes Min) ++ [(ScaleNote Minor 2)]
chordNotes Dom9 = (chordNotes Dom7) ++ [(ScaleNote Major 9)]
chordNotes Dom13 = (chordNotes Dom7) ++ [(ScaleNote Major 13)]
chordNotes Min9 = (chordNotes Min7) ++ [(ScaleNote Minor 9)]
chordNotes Min11 = (chordNotes Min7) ++ [(ScaleNote Minor 11)]
chordNotes Maj9 = (chordNotes Maj7) ++ [(ScaleNote Major 9)]
chordNotes Maj13 = (chordNotes Maj7) ++ [(ScaleNote Major 13)]


--------------------------------------------------------------------------------
-- Interesting functions & queries
--------------------------------------------------------------------------------

--
-- Retrieve all notes of a scale
--   E.g.:
--     Main> scale2notes $ Scale (read "C") Minor
--     [C,D,D#,F,G,G#,A#]
--
scale2notes :: Scale -> [Note]
scale2notes (Scale ground scale) = (init $ scanl (addToNote) ground (intervals scale))

--
-- Retrieve all notes of a chord
--   E.g.: 
--     Main> chord2notes $ Chord (read "C") Sus4
--     [C,F,G]
--
chord2notes :: Chord -> [Note]
chord2notes (Chord note name) = sortBy (compareNotes note) $ map (snote2note note) (chordNotes name)
  where
    snote2note :: Note -> ScaleNote -> Note
    snote2note ground (ScaleNote scale note) = (scale2notesInf (Scale ground scale)) !! (note-1)

--
-- Compute the list of all chords containing given notes
--   E.g.:
--     Main> chordsContaining [(read "C"), (read "G"), (read "E"), (read "D")]
--     [Cadd2,C9,CMaj9,AMin11]
--
chordsContaining :: [Note] -> [Chord]
chordsContaining notes = sort [ c | c <- chords, length (intersect (chord2notes c) notes) == length notes]

--
-- Comptute the ist of all scales containing given chord
--  E.g.
--    Main> scalesContaining (Chord (read "A") Min)
--    [C Major,F Major,G Major,D Minor,E Minor,A Minor,D Dorian,G Dorian, ...
--
scalesContaining :: Chord -> [Scale]
scalesContaining c = sort [ s | s <- scales, length (intersect (chord2notes c) (scale2notes s)) == length (chord2notes c)]


--------------------------------------------------------------------------------
-- Note datastructure and operations
--------------------------------------------------------------------------------

data Note = Note Int deriving Eq
type Interval = Int

instance Show Note where
  show (Note a) = noteNames !! a
    where
      noteNames = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

-- String to Note conversion
instance Read Note where
  readsPrec _ s = 
    case (lookup s noteStrings) of 
      Just i -> [(Note i, "")]
      Nothing -> error ("Invalid note: '" ++ s ++ "'")
    where noteStrings = [("C",0), ("C#",1), ("Db",1), ("D",2), ("D#",3), ("Eb",3), ("E",4), ("F",5), ("F#",6), ("Gb",6), ("G",7), ("G#",8), ("Ab",8), ("A",9), ("A#",10), ("Bb",10), ("B",11)]
  
-- A list of all notes
notes :: [Note]
notes = [Note n | n <- [0..11]]

-- Adds an interval to a note
addToNote :: Note -> Interval -> Note
addToNote (Note n) i = Note ((n + i) `mod` 12)

-- Compares 2 notes with respect to a ground note
compareNotes :: Note -> Note -> Note -> Ordering
compareNotes (Note ground) (Note n1) (Note n2) =
  compare ((n1 - ground) `mod` 12) ((n2 - ground) `mod` 12)


--------------------------------------------------------------------------------
-- Scale datastructures & operations
--------------------------------------------------------------------------------

data Scale = Scale Note ScaleName deriving Eq
data ScaleName = Ionian | Dorian | Phrygian | Lydian | Myxolidian | Aeolian | Locrian | Major | Minor deriving (Show, Eq)
data ScaleNote = ScaleNote ScaleName Interval deriving (Show, Eq)

-- All (relevant) scale names
scaleNames :: [ScaleName]
scaleNames = [Major, Minor, Dorian, Phrygian, Lydian, Myxolidian, Locrian]

-- All (relevant) scales
scales :: [Scale]
scales = [(Scale note name) | note <- notes, name <- scaleNames]

-- Comparing scales
instance Ord Scale where
  compare (Scale _ n1) (Scale _ n2)  = compare (complexity n1) (complexity n2)
    where
      complexity :: ScaleName -> Int
      complexity c = case (findIndex (c ==) scaleNames) of Just i -> i

-- Showing scales
instance Show Scale where
  show (Scale note name) = (show note) ++ ' ':(show name)

scale2notesInf :: Scale -> [Note]
scale2notesInf s = (scale2notes s) ++ (scale2notesInf s)


--------------------------------------------------------------------------------
-- Chord datastructures & operations
--------------------------------------------------------------------------------

data Chord = Chord Note ChordName deriving  Eq
data ChordName = Maj | Min | Maj7 | Min7 | Dom7 | Maj6 | Min6 | Five | Sus4 | Sus2 | MajorAdd2 | MinorAdd2 | Dom9 | Dom13 | Min9 | Maj9 | Maj13 | Min11 deriving Eq

-- List of all chord names in ascending order of 'complexity'
chordNames = [Maj, Min, Maj7, Min7, Dom7, Maj6, Min6, Sus4, Sus2, MajorAdd2, MinorAdd2, Dom9, Dom13, Min9, Maj9, Maj13, Min11, Five]

-- List of all possible chords
chords :: [Chord]
chords = [(Chord note name) | note <- notes, name <- chordNames]

-- For sorting chords
instance Ord Chord where
  compare (Chord _ n1) (Chord _ n2)  = compare (complexity n1) (complexity n2)
    where
      complexity :: ChordName -> Int
      complexity c = case (findIndex (c ==) chordNames) of Just i -> i

-- For showing chord names
instance Show ChordName where
  show c = case (lookup c chordNameStrings) of Just s -> s
    where
      chordNameStrings = [(Maj,""), (Min,"m"), (Maj7,"Maj7"), (Min7, "m7"), (Dom7,"7"), (Maj6, "6"), (Min6,"m6"), (Five,"5"), (Sus4, "sus4"), (Sus2,"sus2"), (MajorAdd2, "add2"), (MinorAdd2,"add2"), (Dom9, "9"), (Dom13, "13"), (Min9, "m9"), (Maj9, "Maj9"), (Maj13, "Maj13"), (Min11, "Min11")]

-- For showing chords
instance Show Chord where
  show (Chord note name) = (show note) ++ (show name)
