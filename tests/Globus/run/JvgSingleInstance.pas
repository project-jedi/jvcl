{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSingleInstance.PAS, released on 2003-01-15.

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

unit JvgSingleInstance;

interface

uses
  Windows, Messages, Classes, Forms, Dialogs, SyncObjs, SysUtils,
  JvComponent;

type
  TJvgSingleInstance = class(TJvComponent)
  private
    FCheckEvent: TEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ semaphore

Var hs:THandle;
begin
  hs:=CreateSemaphore(Nil,0,2,'MyProgSemaphore');
  If GetLastError=ERROR_ALREADY_EXISTS Then halt;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  ReleaseSemaphore(hs,2,0);
end.
}

{ mutex

const
  SAppMutex = 'SomeName';

var
  Mtx: THandle;
  Wait: Longint;

begin
  Application.Initialize;
  Mtx := CreateMutex(nil, True, PChar(SAppMutex));
  try
    Wait := WaitForSingleObject(Mtx, 0);
    if (Wait <> WAIT_TIMEOUT) then
    begin
      OpenMutex(SYNCHRONIZE, False, PChar(SAppMutex));
      try
        Application.CreateForm(TfrmMain, frmMain);
        Application.Run;
      finally
        ReleaseMutex(Mtx);
      end;
    end else
      Exit;
  finally
    CloseHandle(Mtx);
  end;
end.
}

constructor TJvgSingleInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then
    Exit;
  FCheckEvent := TEvent.Create(nil, False, True, ParamStr(0));
  if FCheckEvent.WaitFor(10) <> wrSignaled then
  begin
    Application.MessageBox('Копия данной программы уже запущена. Повторный запуск программы не разрешен.', PChar('Повторный запуск программы ' + ExtractFileName(ParamStr(0))),
      MB_ICONSTOP or MB_OK);
    Halt;
  end;
end;

destructor TJvgSingleInstance.Destroy;
begin
  FCheckEvent.Free;
  inherited Destroy;
end;

end.
