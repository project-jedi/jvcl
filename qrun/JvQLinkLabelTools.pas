{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: LinkTools.pas, released 2002-01-06.

The Initial Developer of the Original Code is David Polberger <dpol@swipnet.se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): ______________________________________.

Current Version: 2.00

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Various common utility routines grouped together in groups, represented by
  non-instantiable classes containing class methods. The design of these classes
  has been influenced by Java's class library.

Known Issues:
  Please see the accompanying documentation.
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQLinkLabelTools;

interface

uses
  SysUtils, Classes,  
  Types, QWindows, 
  JvQTypes;

type
  EGenericToolsError = class(EJVCLException);

  TStaticObject = class(TObject)
  public
    constructor Create; virtual;
  end;

  TStringTools = class(TStaticObject)
  private
    class function RemoveChars(const S: string;
      const Chars: array of Char): string;
  public
    class function RemoveCRLF(const S: string): string;
    class function EndsWith(const S: string; const SubS: string = ' '): Boolean;
    class function BeginsWith(const S: string; const SubS: string = ' '): Boolean;
    class function EscapeBackslashes(const S: string): string;
    class function Replace(OldSubstr, NewSubstr: string; var S: string): Boolean;
  end;

  TGraphicTools = class(TStaticObject)
  public
    class function IsPointInRect(const Rect: TRect; const Point: TPoint): Boolean;
  end;

  TConversionTools = class(TStaticObject)
  public
    class function BoolToYesNo(const B: Boolean): string;
    class function BoolToStr(const B: Boolean): string;
  end;

  TWebTools = class(TStaticObject)
  public
    class function OpenWebPage(const URI: string): Boolean;
  end;

  TOwnerPointerList = class(TObject)
  private
    function GetCount: Integer;
  protected
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Count: Integer read GetCount;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  ShellAPI,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX}
  JvQResources;

//=== TStaticObject ==========================================================

constructor TStaticObject.Create;
begin
  raise EGenericToolsError.CreateRes(@RsECannotBeInstantiated);
end;

//=== TStringTools ===========================================================

class function TStringTools.BeginsWith(const S, SubS: string): Boolean;
begin
  Result := Copy(S, 1, Length(SubS)) = SubS;
end;

class function TStringTools.EndsWith(const S, SubS: string): Boolean;
begin
  Result := Copy(S, Length(S) - Length(SubS) + 1, Length(SubS)) = SubS;
end;

class function TStringTools.RemoveChars(const S: string;
  const Chars: array of Char): string;
begin
  Result := StringReplace(S, Chars, '', [rfReplaceAll, rfIgnoreCase]);
end;

class function TStringTools.RemoveCRLF(const S: string): string;
begin
  Result := RemoveChars(S, [#13, #10]);
end;

class function TStringTools.EscapeBackslashes(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = '\' then
      Insert('\', Result, I);
end;

class function TStringTools.Replace(OldSubstr, NewSubstr: string;
  var S: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  while AnsiPos(OldSubstr, S) <> 0 do
  begin
    Pos := AnsiPos(OldSubstr, S);
    Result := Pos <> 0;

    if Result then
    begin
      Delete(S, Pos, Length(OldSubstr));
      Insert(NewSubstr, S, Pos);
    end;
  end;
end;

//=== TGraphicTools ==========================================================

class function TGraphicTools.IsPointInRect(const Rect: TRect;
  const Point: TPoint): Boolean;
begin
  Result :=
    (Point.X >= Rect.Left) and (Point.X <= Rect.Right) and
    (Point.Y >= Rect.Top) and (Point.Y <= Rect.Bottom);
end;

//=== TConversionTools =======================================================

class function TConversionTools.BoolToStr(const B: Boolean): string;
begin
  if B then
    Result := 'True'
  else
    Result := 'False';
end;

class function TConversionTools.BoolToYesNo(const B: Boolean): string;
begin
  if B then
    Result := 'Yes'
  else
    Result := 'No';
end;

//=== TWebTools ==============================================================

class function TWebTools.OpenWebPage(const URI: string): Boolean;
begin
  Result := ShellExecute(0, 'open', PChar(URI), nil, nil, SW_SHOWNORMAL) > 32;
end;

//=== TOwnerPointerList ======================================================

constructor TOwnerPointerList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TOwnerPointerList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TOwnerPointerList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    Dispose(FList[I]);
  FList.Clear;
end;

function TOwnerPointerList.GetCount: Integer;
begin
  Result := FList.Count;
end;

end.
