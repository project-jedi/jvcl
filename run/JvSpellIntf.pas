{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpellIntf.PAS, released on 2003-08-19.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Interface declarations for classes that want to implement a spell
  checker compatible with the TJvSpellChecker component.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSpellIntf;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes;

type
  TJvSpellCheckIgnoreEvent = procedure(Sender: TObject; const Value: string;
    var CanIgnore: Boolean) of object;

  IJvSpellChecker = interface
    ['{819CE37A-E3C1-4F54-B9E1-1CFAA8AFB887}']
    // GetCurrentWord returns the currently found misspelled or missing word
    function GetCurrentWord: string;
    // Seek moves the internal text pointer to the position in Text given by Position.
    // If Position >= Length(Text), calls to Next always returns false.
    // Since positioning is 1-based, Seek(0) = Seek(1).
    procedure Seek(Position: Integer);
    // Next returns true if a misspelled word was found. If a misspelled word
    // was found, StartIndex is set to the start of the word in Text and WordLength
    // is set to the length of the word. Note that StartIndex is 1-based, i.e the first
    // position in Text is 1. If Next returns false, it means that no more misspelled words
    // can be found (i.e either when at end of Text or everything from the current point and
    // onward is correctly spelled)
    function Next(out StartIndex, WordLength: Integer): WordBool;
    // The Text to spell check. When Text is changed, the internal position is reset
    // to the start of Text (no need to call Seek)
    function GetText: string;
    procedure SetText(const Value: string);
    property Text: string read GetText write SetText;
    // Delimiters specifies the characters that are used to break strings into words.
    function GetDelimiters: TSysCharSet;
    procedure SetDelimiters(const Value: TSysCharSet);
    property Delimiters: TSysCharSet read GetDelimiters write SetDelimiters;

    // Adds the content of a dictionary to the internal list of words that are scanned for matches.
    procedure SetDictionary(const Value: string);
    function GetDictionary: string;
    property Dictionary: string read GetDictionary write SetDictionary;
    // "User" dictionary. This is a list of words, sorted.
    // Manage the user dictionary by using the methods of TStrings.
    // The main difference between a dictionary and a user dictionary is that you cannot change
    // the content of the main dictionary from the interface. In addition, the UserDictionary is presumed to
    // contain a list of words, one per line, sorted whereas the dictionary can be in any format (determined
    // by the actual implementation).
    function GetUserDictionary: TStrings;
    procedure SetUserDictionary(const Value: TStrings);
    property UserDictionary: TStrings read GetUserDictionary write SetUserDictionary;
    // Ignores are used for words that should be ignored in the current session.
    // To make an ignore persistent, you should call UserDictionary.Add
    // and then save/load from file as needed.
    function GetIgnores: TStrings;
    procedure SetIgnores(const Value: TStrings);
    property Ignores: TStrings read GetIgnores write SetIgnores;
    // Suggestion returns the suggested replacements for a misspelled word. How the
    // implementation determines valid and/or useful replacement words is defined
    // by the implementation.
    function GetSuggestions: TStrings;
    property Suggestions: TStrings read GetSuggestions;
    // Assign a handler to this event when you need to set up ignores for words
    // that can't be captured using the ignore list and/or the user dictionary.
    function GetCanIgnore: TJvSpellCheckIgnoreEvent;
    procedure SetCanIgnore(const Value: TJvSpellCheckIgnoreEvent);
    property OnCanIgnore: TJvSpellCheckIgnoreEvent read GetCanIgnore write SetCanIgnore;
  end;

var
  CreateSpellChecker: function: IJvSpellChecker = nil;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

