{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShBrProperty.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Property editor for the TJvShellBrowse component }

unit JvShBrProperty;
interface
uses
  {$IFDEF Delphi6_UP}DesignEditors,DesignIntf{$ELSE}DsgnIntf{$ENDIF};
type
  TJvShellBrowserEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

resourcestring
  SEditProperty = 'Preview...';


implementation
uses
  JvShBrowse;

{ TJvShellBrowserEditor }

procedure TJvShellBrowserEditor.Edit;
begin
 {$IFDEF Delphi6_UP}
  (GetComponent as TJvShellBrowser).Execute;
{$ELSE}
if Component is TJvShellBrowser then
TJvShellBrowser(Component).Execute;
{$ENDIF}


end;

procedure TJvShellBrowserEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit else inherited;
end;

function TJvShellBrowserEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := SEditProperty
  else
    Result := inherited GetVerb(Index);
end;

function TJvShellBrowserEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
