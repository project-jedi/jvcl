{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit ClassScript;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ClassUtils, ClassQueryDisplay, ClassUrlParser, jpeg, ExtCtrls;

type
  TScriptHandler = class
  private
    FResult: TStream;
    FScript: TStream;
    FCurrentPath: string;
    FParams: TUrlParser;
    FHeaders: TStringList;
    FCurrentUser: Integer;
  protected
    procedure AnalyseScript(var AScript: string);
    procedure ReadData;
    procedure ReadScript;

    procedure DoResizeImage(AParams: string);

    function GetPath(ACurrentPath, ANewPath: string): string;
  public
    procedure Request(AFileName, AParams: string; AResult: TStream;
      AHeaders: TStringList);
    function GetParam(const AName: string): string;

    property CurrentUser: Integer read FCurrentUser write FCurrentUser;
  end;

implementation

uses
  ClassHospital, JvXmlDatabase;


{ TScriptHandler }

{**********************************************************************}
procedure TScriptHandler.AnalyseScript(var AScript: string);
var
 lToken, st: string;
 lQuery: TJvXmlQuery;
 i: Integer;
 lStream: TFileStream;

  function ReadParam: string;
  begin
    result := '';
    if (i<Length(AScript)) and (AScript[i]='(') then
    begin
      inc(i);
      if (i<Length(AScript)) and (AScript[i]='"') then
      begin
        inc(i);
        while i <= Length(AScript) do
        begin
          if AScript[i] = '"' then
          begin
            if AScript[i-1] = '\' then //Use \ as escape char
            begin
              SetLength(result, Length(result) - 1);
              result := result + '"';
            end
            else
              Exit;
          end
          else
            result := result + AScript[i];
          inc(i);
        end;
      end;
    end;
  end;

  function ParseParams(AValue: string): string;
  var
   i: Integer;
  begin
    result := AValue;
    for i:=0 to FParams.Count-1 do
      result := StringReplace(result, '$' + FParams.Variable[i].Name,
        StringReplace(FParams.Variable[i].Value, '''', '\''', [rfReplaceAll]),
          [rfReplaceAll, rfIgnoreCase]); //Replace by quoted str
  end;

begin

{<%
  Request("SELECT * FROM users.xml");
  Display("userstable.xml");
%>}

  lToken := '';
  i := 1;
  lQuery := nil;

  FParams.AddVariable('curuser', IntToStr(CurrentUser)); 

  while i <= Length(AScript) do
  begin
    case AScript[i] of
      'a'..'z', 'A'..'Z':
        lToken := lToken + AScript[i];
      '(':
        begin
          lToken := LowerCase(lToken);
          if lToken = 'request' then
          begin
            st := ReadParam;
            st := ParseParams(st);
            if lQuery<>nil then
              lQuery.Free;
            lQuery := GHospital.Database.Query(st);
            FParams.AddVariable('lastid', IntToStr(lQuery.LastId));
          end
          else if lToken = 'display' then
          begin
            st := GetPath(FCurrentPath, ReadParam);
            with TQueryDisplayer.Create(self) do
            try
              Generate(lQuery, st, FResult);
            finally
              Free;
            end;
          end
          else if lToken = 'echo' then
          begin
            st := ReadParam;
            st := ParseParams(st);
            FResult.Write(st[1], Length(st));
          end
          else if lToken = 'displayempty' then
          begin
            if lQuery.Results.Items.Count = 0 then
            begin
              st := GetPath(FCurrentPath, ReadParam);
              with TStringList.Create do
              try
                LoadFromFile(st);
                st := ParseParams(Text);
                FResult.Write(st[1], Length(st));
              finally
                Free;
              end;
            end;
          end
          else if lToken = 'displaynonempty' then
          begin
            if lQuery.Results.Items.Count > 0 then
            begin
              st := GetPath(FCurrentPath, ReadParam);
              with TStringList.Create do
              try
                LoadFromFile(st);
                st := ParseParams(Text);
                FResult.Write(st[1], Length(st));
              finally
                Free;
              end;
            end;
          end
          else if lToken = 'include' then
          begin
            st := ReadParam;
            st := GetPath(FCurrentPath, st);
            if FileExists(st) then
            begin
              lStream := TFileStream.Create(st, fmOpenRead);
              try
                FResult.CopyFrom(lStream, 0);
              finally
                lStream.Free;
              end;
            end;
          end
          else if lToken = 'resizeimage' then
          begin
            st := ReadParam;
            DoResizeImage(st);
          end
          else if lToken = 'redirect' then
          begin
            st := ReadParam;
            FHeaders.Add('Location: ' + ParseParams(st));
          end
          else
            raise Exception.Create('Unknown token '+lToken);
          lToken := '';
        end;
      ' ',#9,#10,#13,')',';':
        begin
          //Just ignore them for the lightness of the parser
        end;
    end;

    inc(i);
  end;

  if lQuery<>nil then
    lQuery.Free;
end;
{**********************************************************************}
procedure TScriptHandler.DoResizeImage(AParams: string);
var
 lFile: string;
 lWidth, lHeight, i: integer;
 lJpeg: TJPEGImage;
 lBmp: TBitmap;
begin
  //Limited to jpeg/bmp images
  try
    i := pos(',', AParams);
    lWidth := -1;
    lHeight := -1;
    if i<>0 then
    begin
      lFile := Copy(AParams, 1, i-1);
      AParams := Copy(AParams, i+1, MAXINT);

      for i:=0 to FParams.Count-1 do
        if FParams.Variable[i].Name = lFile then
        begin
          lFile := FParams.Variable[i].Value;
          Break;
        end;

      lFile := GetPath(FCurrentPath, lFile);
      i := pos(',', AParams);
      if i<>0 then
      begin
        lWidth := StrToIntDef(Copy(AParams, 1, i-1), -1);
        lHeight := StrToIntDef(Copy(AParams, i+1, MAXINT), -1);
      end
      else
        lWidth := StrToIntDef(AParams, -1);
    end
    else
      lFile := AParams;

    lJpeg := TJPEGImage.Create;
    lBmp := TBitmap.Create;
    with TImage.Create(nil) do
    try
      Picture.LoadFromFile(lFile);

      Proportional := true;
      Width := lWidth;
      Height := lHeight;

      lBmp.Width := lWidth;
      lBmp.Height := lHeight;
      lBmp.Canvas.StretchDraw(Rect(0,0,lWidth,lHeight),Picture.Graphic);

      lJpeg.Assign(lBmp);
      lJpeg.SaveToStream(FResult);
    finally
      lJpeg.Free;
      lBmp.Free;
      Free;
    end;
  except
  end;
end;
{**********************************************************************}
function TScriptHandler.GetParam(const AName: string): string;
var
 i: Integer;
begin
  result := '';
  for i:=0 to FParams.Count-1 do
    if FParams.Variable[i].Name = AName then
    begin
      result := FParams.Variable[i].Value;
      Exit;
    end;
end;
{**********************************************************************}
function TScriptHandler.GetPath(ACurrentPath, ANewPath: string): string;
var
 st: string;
begin
  {$IFDEF MSWINDOWS}
  ACurrentPath := StringReplace(ACurrentPath,'/','\',[rfReplaceAll]);
  ANewPath := StringReplace(ANewPath,'/','\',[rfReplaceAll]);
  {$ENDIF}
  if FileExists(ANewPath) then
    result := ANewPath
  else
  begin
    st := GetCurrentDir;

    SetCurrentDir(ExtractFilePath(ACurrentPath));
    result := ExpandFileName(ANewPath);

    SetCurrentDir(st);
  end;
end;
{**********************************************************************}
procedure TScriptHandler.ReadData;
var
 lBuffer: array[0..1024] of char;
 lCount, i, lPos: Integer;
begin
  lPos := 0;
  while FScript.Position < FScript.Size do
  begin
    lCount := FScript.Read(lBuffer, SizeOf(lBuffer));
    for i:=0 to lCount-1 do
      case lBuffer[i] of
        '<':
          lPos := 1;
        '%':
          if lPos = 1 then
          begin
            FResult.Write(lBuffer, i - 1);
            FScript.Seek(-(lCount - (i + 1)),soFromCurrent);
            Exit;
          end;
        else
          lPos := 0;
      end;
    FResult.Write(lBuffer, lCount);
  end;
end;
{**********************************************************************}
procedure TScriptHandler.ReadScript;
var
 lBuffer: array[0..1024] of char;
 lCount, i, lPos: Integer;
 lScript: string;
begin
  lPos := 0;
  lScript := '';
  while FScript.Position < FScript.Size do
  begin
    lCount := FScript.Read(lBuffer, SizeOf(lBuffer));
    for i:=0 to lCount-1 do
      case lBuffer[i] of
        '%':
          lPos := 1;
        '>':
          if lPos = 1 then
          begin
            FScript.Seek(-(lCount - (i + 1)),soFromCurrent);
            AnalyseScript(lScript);
            Exit;
          end
          else
            lScript := lScript + lBuffer[i];
        else
        begin
          if lPos = 1 then
            lScript := lScript + '%';
          lScript := lScript + lBuffer[i];
          lPos := 0;
        end;
      end;
  end;
end;
{**********************************************************************}
procedure TScriptHandler.Request(AFileName, AParams: string;
  AResult: TStream; AHeaders: TStringList);
begin
  FScript := TFileStream.Create(AFileName, fmOpenRead and fmShareDenyWrite);
  FParams := TUrlParser.Create('?' + AParams);
  FCurrentPath := ExtractFilePath(AFileName);
  FResult := AResult;
  FHeaders := AHeaders;
  try
    while FScript.Position < FScript.Size do
    begin
      ReadData;
      ReadScript;
    end;
  finally
    FScript.Free;
    FParams.Free;
  end;
end;
{**********************************************************************}
end.

