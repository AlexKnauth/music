\version "2.24.4"
% automatically converted by musicxml2ly from /Users/Alex/git/AlexKnauth/music/music/demo/Bach-Goldberg-Canone-alla-Quarta.xml
\pointAndClickOff

\header {
    title =  "Goldberg Variations"
    movementnumber =  "12"
    subtitle =  "Canone alla Quarta"
    composer =  Bach
    }

\layout {
    \context { \Score
        skipBars = ##t
        autoBeaming = ##f
        }
    }
PartPOneVoiceOne =  \relative g' {
    | % 1
    \clef "treble" \key g \major \time 3/4 \tempo 4=80 r8 g16 fis16 g8 a16
    b16 c16 b16 a16 g16 | % 2
    d'16 a16 b16 cis16 d16 e16 fis16 g16 a8 r8 | % 3
    r8 g16 fis16 e16 d16 cis16 b16 a16 g16 fis16 g16 | % 4
    g8 fis16 e16 d8 r4. | % 5
    r16 d'8. d16 b16 a16 g16 f16 e16 d16 f16 | % 6
    e16 g16 c16 d16 e8 a,4 a8 | % 7
    a16 d,16 fis16 g16 a16 g16 fis16 e'16 d16 c16 b16 a16 | % 8
    g4 r2 }

PartPTwoVoiceOne =  \relative d' {
    | % 1
    \clef "treble" \key g \major \time 3/4 \tempo 4=80 R2. | % 2
    r8 d16 e16 d8 c16 b16 a16 b16 c16 d16 | % 3
    g,16 c16 b16 a16 g16 fis16 e16 d16 cis8 r8 | % 4
    r8 d16 e16 fis16 g16 a16 b16 c16 d16 e16 d16 | % 5
    d8 e16 fis16 g8 r4. | % 6
    r16 g,8. g16 b16 c16 d16 e16 fis16 g16 e16 | % 7
    fis16 d16 a16 g16 fis8 c'4 c8 | % 8
    c16 g'16 e16 d16 c16 d16 e16 fis,16 g16 a16 b16 c16 | % 9
    d4 r2 }

PartPThreeVoiceOne =  \relative g {
    | % 1
    \clef "bass" \key g \major \time 3/4 \tempo 4=80 g4 g4 g4 | % 2
    fis4 fis4 fis4 | % 3
    e4 e4 e4 | % 4
    d16 d,16 d'8 r8 c16 b16 a8 c8 | % 5
    b4 b4 b4 | % 6
    c4 c4 c4 | % 7
    d4 d4 d4 | % 8
    g8. b,16 e16 d16 c16 e16 d16 c16 b16 a16 | % 9
    g4 r2 }

PartPFourVoiceOne =  \relative g {
    | % 1
    \key g \major \time 3/4 <g b g'>2. ^\markup { \fret-diagram
        #"6-x;5-x;4-x;3-o;2-o;1-3;" } | % 2
    <d a' c fis>2. ^\markup { \fret-diagram
        #"h:3;6-x;5-x;4-o;3-2;2-1;1-2;" } | % 3
    <a e' a cis e>2. ^\markup { \fret-diagram
        #"h:3;6-x;5-o;4-2;3-2;2-2;1-o;" } | % 4
    <d a' c fis>2. ^\markup { \fret-diagram
        #"h:3;6-x;5-x;4-o;3-2;2-1;1-2;" } | % 5
    <g, b d g b g'>2. ^\markup { \fret-diagram
        #"6-3;5-2;4-o;3-o;2-o;1-3;" } | % 6
    <c e g c e>4. ^\markup { \fret-diagram #"6-x;5-3;4-2;3-o;2-1;1-o;" }
    <a e' a c e>4. ^\markup { \fret-diagram
        #"h:3;6-x;5-o;4-2;3-2;2-1;1-o;" } | % 7
    <d a' c fis>2. ^\markup { \fret-diagram
        #"h:3;6-x;5-x;4-o;3-2;2-1;1-2;" } | % 8
    <c e g c e>2 ^\markup { \fret-diagram #"6-x;5-3;4-2;3-o;2-1;1-o;" }
    <g b d g b g'>8. ^\markup { \fret-diagram
        #"6-3;5-2;4-o;3-o;2-o;1-3;" } <a e' a c e>16 ^\markup {
        \fret-diagram #"h:3;6-x;5-o;4-2;3-2;2-1;1-o;" } | % 9
    <g b d g b g'>4 ^\markup { \fret-diagram #"6-3;5-2;4-o;3-o;2-o;1-3;"
        } r2 }

PartPFourVoiceOneChords =  \chordmode {
    | % 1
    g2.:5 | % 2
    d2.:7 | % 3
    a2.:7 | % 4
    d2.:7 | % 5
    g2.:7 | % 6
    c4.:5 a4.:m5 | % 7
    d2.:7 | % 8
    c2:5 g8.:5 a16:m5 | % 9
    g4:5 }


% The score definition
\score {
    <<
        
        \new Staff
        <<
            \set Staff.instrumentName = "Melody"
            
            \context Staff << 
                \mergeDifferentlyDottedOn\mergeDifferentlyHeadedOn
                \context Voice = "PartPOneVoiceOne" {  \PartPOneVoiceOne }
                >>
            >>
        \new Staff
        <<
            \set Staff.instrumentName = "Melody-Transformed"
            
            \context Staff << 
                \mergeDifferentlyDottedOn\mergeDifferentlyHeadedOn
                \context Voice = "PartPTwoVoiceOne" {  \PartPTwoVoiceOne }
                >>
            >>
        \new Staff
        <<
            \set Staff.instrumentName = "Bassline"
            
            \context Staff << 
                \mergeDifferentlyDottedOn\mergeDifferentlyHeadedOn
                \context Voice = "PartPThreeVoiceOne" {  \PartPThreeVoiceOne }
                >>
            >>
        \context ChordNames = "PartPFourVoiceOneChords" { \PartPFourVoiceOneChords}
        \new Staff
        <<
            \set Staff.instrumentName = "Guitar"
            
            \context Staff << 
                \mergeDifferentlyDottedOn\mergeDifferentlyHeadedOn
                \context Voice = "PartPFourVoiceOne" {  \PartPFourVoiceOne }
                >>
            >>
        
        >>
    \layout {}
    % To create MIDI output, uncomment the following line:
    %  \midi {\tempo 4 = 80 }
    }

