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

UNIT JvgProcess;                        //...simple process managment

INTERFACE

USES Windows,
   Messages,
   JVComponent,
   Classes,
   Forms,
   dialogs;

TYPE

   TJvgProcess = CLASS(TJvComponent)
   PRIVATE
      FResult: boolean;
      FFileName: STRING;
      FOnTermainated: TNotifyEvent;
      si: TStartupInfo;
   PUBLIC
      pi: TProcessInformation;
      FUNCTION Run: boolean;
      FUNCTION Kill: boolean;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY FileName: STRING READ FFileName WRITE FFileName;
      PROPERTY Result: boolean READ FResult STORED false;
      PROPERTY OnTermainated: TNotifyEvent READ FOnTermainated WRITE
         FOnTermainated;
   END;

PROCEDURE Register;

IMPLEMENTATION

PROCEDURE Register;
BEGIN
END;

DESTRUCTOR TJvgProcess.Destroy;
BEGIN
   Kill;
   INHERITED;
END;

FUNCTION TJvgProcess.Run: boolean;
BEGIN
   GetStartupInfo(si);
   si.wShowWindow := SW_NORMAL;
   FResult := CreateProcess(PChar(FFileName), NIL, NIL, NIL, false,
      NORMAL_PRIORITY_CLASS, NIL, NIL, si, pi);
   Run := FResult;
   IF Result THEN
   BEGIN
      WHILE WaitForSingleObject(pi.hProcess, 100) = WAIT_TIMEOUT DO
         Application.ProcessMessages;
      IF Assigned(OnTermainated) THEN
         OnTermainated(self);
   END;
END;

FUNCTION TJvgProcess.Kill: boolean;
BEGIN
   IF FResult {and(WaitForSingleObject(pi.hProcess, 100) <> WAIT_TIMEOUT)} THEN
      Kill := TerminateProcess(pi.hProcess, 0)
   ELSE
      Kill := false;
END;

END.

