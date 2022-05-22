module Data.Pica.Arbitrary () where

import Data.Char (isDigit)
import Data.Pica
import qualified Data.Text as T
import Test.QuickCheck

instance Arbitrary Record where
  arbitrary = Record <$> listOf1 arbitrary

instance Arbitrary Field where
  arbitrary = Field <$> arbitraryTag <*> arbitraryOccurrence <*> arbitrary

arbitraryOccurrence :: Gen (Maybe T.Text)
arbitraryOccurrence =
  frequency
    [ (7, return Nothing),
      ( 2,
        do
          p0 <- arbitrary `suchThat` isDigit
          p1 <- arbitrary `suchThat` isDigit
          return $ Just (T.pack [p0, p1])
      ),
      ( 1,
        do
          p0 <- arbitrary `suchThat` isDigit
          p1 <- arbitrary `suchThat` isDigit
          p2 <- arbitrary `suchThat` isDigit
          return $ Just (T.pack [p0, p1, p2])
      )
    ]

arbitraryTag :: Gen T.Text
arbitraryTag = do
  c0 <- choose ('0', '2')
  c1 <- choose ('0', '9')
  c2 <- choose ('0', '9')
  c3 <- elements ('@' : ['A' .. 'Z'])
  return $ T.pack [c0, c1, c2, c3]

instance Arbitrary Subfield where
  arbitrary = do
    value <- arbitrary `suchThat` (\s -> '\RS' `notElem` s && '\US' `notElem` s)
    -- The following frequency distribution is based on an evaluation
    -- of 36,004,180 PICA+ record of the German National Library (DNB).
    code <-
      frequency
        [ (906253807, return 'a'),
          (667064726, return '0'),
          (257368148, return 'b'),
          (88113262, return 't'),
          (84626978, return 'd'),
          (77701657, return '9'),
          (70031802, return 'e'),
          (69065585, return '4'),
          (61328378, return 'A'),
          (54838520, return 'x'),
          (54272758, return 'D'),
          (53994225, return '7'),
          (53342833, return 'V'),
          (53122587, return 'E'),
          (52560961, return 'H'),
          (51481491, return 'S'),
          (46754544, return 'g'),
          (43042661, return 'B'),
          (39457210, return 'c'),
          (34489490, return 'h'),
          (27377425, return 'f'),
          (25957448, return 'n'),
          (24677143, return 'p'),
          (20875688, return 'K'),
          (19484506, return 'L'),
          (18770595, return 'j'),
          (18589996, return 'm'),
          (17466569, return 'u'),
          (16589916, return 'Y'),
          (16248510, return '5'),
          (14563958, return 'r'),
          (13636217, return '6'),
          (13008440, return 'i'),
          (11690761, return 'v'),
          (11415175, return 'I'),
          (10830922, return 'T'),
          (10254059, return 'G'),
          (10103239, return 'l'),
          (8087233, return 'q'),
          (8048346, return 'F'),
          (7251404, return 'Z'),
          (6592821, return 'z'),
          (5231486, return 'o'),
          (4245083, return 'k'),
          (4048000, return 'y'),
          (3974880, return 'J'),
          (3491921, return 'N'),
          (2737530, return 'O'),
          (2479479, return 'M'),
          (1590378, return '2'),
          (1476197, return 's'),
          (1461367, return 'R'),
          (1331980, return 'P'),
          (734868, return 'U'),
          (472791, return 'X'),
          (455203, return '3'),
          (21692, return 'w'),
          (20308, return 'Q'),
          (6698, return 'C'),
          (13, return '1'),
          (1, return '8'),
          (1, return 'W')
        ]
    return $ Subfield code (T.pack value)
