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

unit MyLabel;

interface

uses Classes, StdCtrls;

type

   TMyLabel = class(TLabel)
   private
     function GetSomeProperty: String;
     procedure SetSomeProperty(Value: String);
   public
     procedure DoSomething;
     property SomeProperty: String read GetSomeProperty write SetSomeProperty;
   end;

  procedure Register;

implementation

{ TMyLabel }

procedure TMyLabel.DoSomething;
begin
  Caption := 'DoSomething';
end;

procedure Register;
begin
  RegisterComponents('JVCL', [TMyLabel]);
end;

function TMyLabel.GetSomeProperty: String;
begin
  Result := Caption;
end;

procedure TMyLabel.SetSomeProperty(Value: String);
begin
  Caption := Value;
end;

end.
