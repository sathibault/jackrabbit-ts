package lstestutil

import (
	"fmt"
	"strings"

	"github.com/microsoft/typescript-go/internal/core"
)

type markerRange struct {
	core.TextRange
	filename string
	position int
	data     string
}

type Marker struct {
	Filename string
	Position int
	Name     string
}

type TestData struct {
	Files           []*TestFileInfo
	MarkerPositions map[string]*Marker
	//markers         []*Marker
	/**
	 * Inserted in source files by surrounding desired text
	 * in a range with `[|` and `|]`. For example,
	 *
	 * [|text in range|]
	 *
	 * is a range with `text in range` "selected".
	 */
	Ranges markerRange
}

func ParseTestData(basePath string, contents string, fileName string) TestData {
	// List of all the subfiles we've parsed out
	var files []*TestFileInfo

	// Split up the input file by line
	lines := strings.Split(contents, "\n")
	currentFileContent := ""

	for _, line := range lines {
		if len(line) > 0 && line[len(line)-1] == '\r' {
			line = line[:len(line)-1]
		}
		if currentFileContent == "" {
			currentFileContent = line
		} else {
			currentFileContent += "\n" + line
		}
	}

	if currentFileContent == "" {
		return TestData{}
	}
	markerPositions := make(map[string]*Marker)
	markers := []*Marker{}

	// If we have multiple files, then parseFileContent needs to be called for each file.
	// This will be achieved by creating a `nextFile()` func that will call `parseFileContent()` for each file.
	testFileInfo := parseFileContent(currentFileContent, fileName, markerPositions, &markers)
	files = append(files, testFileInfo)

	return TestData{
		Files:           files,
		MarkerPositions: markerPositions,
		// markers:         markers,
		Ranges: markerRange{},
	}
}

type locationInformation struct {
	position       int
	sourcePosition int
	sourceLine     int
	sourceColumn   int
}

type TestFileInfo struct { // for FourSlashFile
	Filename string
	// The contents of the file (with markers, etc stripped out)
	Content string
}

func parseFileContent(content string, filename string, markerMap map[string]*Marker, markers *[]*Marker) *TestFileInfo {
	// !!! chompLeadingSpace
	// !!! validate characters in markers
	// Any slash-star comment with a character not in this string is not a marker.
	// const validMarkerChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$1234567890_"

	/// The file content (minus metacharacters) so far
	output := ""

	/// The total number of metacharacters removed from the file (so far)
	difference := 0

	/// Current position data
	line := 1
	column := 1

	/// The current marker (or maybe multi-line comment?) we're parsing, possibly
	var openMarker locationInformation

	/// The latest position of the start of an unflushed plain text area
	lastNormalCharPosition := 0

	flush := func(lastSafeCharIndex int) {
		if lastSafeCharIndex != -1 {
			output = output + content[lastNormalCharPosition:lastSafeCharIndex]
		} else {
			output = output + content[lastNormalCharPosition:]
		}
	}

	previousCharacter := content[0]
	for i := 1; i < len(content); i++ {
		currentCharacter := content[i]
		if previousCharacter == '/' && currentCharacter == '*' {
			// found a possible marker start
			openMarker = locationInformation{
				position:       (i - 1) - difference,
				sourcePosition: i - 1,
				sourceLine:     line,
				sourceColumn:   column,
			}
		}
		if previousCharacter == '*' && currentCharacter == '/' {
			// Record the marker
			// start + 2 to ignore the */, -1 on the end to ignore the * (/ is next)
			markerNameText := strings.TrimSpace(content[openMarker.sourcePosition+2 : i-1])
			recordMarker(filename, openMarker, markerNameText, markerMap, markers)

			flush(openMarker.sourcePosition)
			lastNormalCharPosition = i + 1
			difference += i + 1 - openMarker.sourcePosition

			// Set the current start to point to the end of the current marker to ignore its text
			openMarker = locationInformation{}
		}
		if currentCharacter == '\n' && previousCharacter == '\r' {
			// Ignore trailing \n after \r
			continue
		} else if currentCharacter == '\n' || currentCharacter == '\r' {
			line++
			column = 1
			continue
		}

		column++
		previousCharacter = currentCharacter
	}

	// Add the remaining text
	flush(-1)

	return &TestFileInfo{
		Content:  output,
		Filename: filename,
	}
}

func recordMarker(
	filename string,
	location locationInformation,
	name string,
	markerMap map[string]*Marker,
	markers *[]*Marker,
) *Marker {
	// Record the marker
	marker := &Marker{
		Filename: filename,
		Position: location.position,
		Name:     name,
	}
	// Verify markers for uniqueness
	if _, ok := markerMap[name]; ok {
		fmt.Printf("Duplicate marker name: %s\n", name) // tbd print error msg
	} else {
		markerMap[name] = marker
		(*markers) = append(*markers, marker)
	}
	return marker
}
