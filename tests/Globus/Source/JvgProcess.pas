{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgProcess.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgProcess; //...simple process managment

interface

uses Windows,
  Messages,
  JVComponent,
  Classes,
  Forms,
  dialogs;

type

  TJvgProcess = class(TJvComponent)
  private
    FResult: boolean;
    FFileName: string;
    FOnTermainated: TNotifyEvent;
    si: TStartupInfo;
  public
    pi: TProcessInformation;
    function Run: boolean;
    function Kill: boolean;
    destructor Destroy; override;
  published
    property FileName: string read FFileName write FFileName;
    property Result: boolean read FResult stored false;
    property OnTermainated: TNotifyEvent read FOnTermainated write
      FOnTermainated;
  end;

procedure Register;

implementation

procedure Register;
begin
end;

destructor TJvgProcess.Destroy;
begin
  Kill;
  inherited;
end;

function TJvgProcess.Run: boolean;
begin
  GetStartupInfo(si);
  si.wShowWindow := SW_NORMAL;
  FResult := CreateProcess(PChar(FFileName), nil, nil, nil, false,
    NORMAL_PRIORITY_CLASS, nil, nil, si, pi);
  Run := FResult;
  if Result then
  begin
    while WaitForSingleObject(pi.hProcess, 100) = WAIT_TIMEOUT do
      Application.ProcessMessages;
    if Assigned(OnTermainated) then
      OnTermainated(self);
  end;
end;

function TJvgProcess.Kill: boolean;
begin
  if FResult {and(WaitForSingleObject(pi.hProcess, 100) <> WAIT_TIMEOUT)} then
    Kill := TerminateProcess(pi.hProcess, 0)
  else
    Kill := false;
end;

end.
