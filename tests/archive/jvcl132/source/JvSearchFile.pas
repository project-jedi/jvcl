{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSearchFile.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

unit JvSearchFile;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvTypes, JvComponent, FileCtrl;

type
  TJvSearchFile = class(TJvComponent)
  private
    FRecursive: Boolean;
    FMask: string;
    FOnFound: TOnFound;
    FOnChangedDir: TOnChangedDir;
    FOnStart: TNotifyEvent;
    FOnEnd: TNotifyEvent;
    procedure Search(StartPath: string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    procedure Execute(StartPath: string);

    property Mask: string read FMask write FMask;
    property Recursive: Boolean read FRecursive write FRecursive default True;

    property OnFound: TOnFound read FOnFound write FOnFound;
    property OnChangedDir: TonChangedDir read FOnChangedDir write FOnChangedDir;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnEnded: TNotifyEvent read Fonend write FOnEnd;
  end;

implementation

{*****************************************************}

constructor TJvSearchFile.Create(AOwner: TComponent);
begin
  inherited;
  FRecursive := True;
  FMask := '*.exe';
end;

{*****************************************************}

procedure TJvSearchFile.Search(StartPath: string);
var
  t: TSearchRec;
  res: Integer;
begin
  if Assigned(FOnFound) then
  begin
    if Assigned(FOnChangedDir) then
      FOnChangedDir(Self, StartPath);

    res := FindFirst(StartPath + FMask, faAnyFile, t);
    while res = 0 do
    begin
      if (t.name <> '.') and (t.name <> '..') then
        FOnFound(Self, StartPath + t.name);
      res := FindNext(t);
    end;
    FindClose(t);

    if FRecursive then
    begin
      res := FindFirst(StartPath + '*.*', faAnyFile, t);
      while res = 0 do
      begin
        if (t.Name <> '.') and (t.Name <> '..') then
          if (DirectoryExists(StartPath + t.Name + '\')) then
            Search(StartPath + t.Name + '\');
        res := FindNext(t);
      end;
      FindClose(t);
    end;
  end;
end;

{*****************************************************}

procedure TJvSearchFile.Execute(StartPath: string);
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
  // (rom) bug deleted. No OnEnd if StartPath = ''
  if StartPath <> '' then
  begin
    if StartPath[Length(StartPath)] <> '\' then
      StartPath := StartPath + '\';
    Search(StartPath);
  end;
  if Assigned(FOnEnd) then
    FOnEnd(Self);
end;

end.
