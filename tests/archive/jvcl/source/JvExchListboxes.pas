{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExchListboxes.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvExchListboxes;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFormLists, JvBaseDlg, JvTypes;

type
  TJvExchListboxes = class(TJvCommonDialog)
  private
    FCaption2: string;
    FCaption: string;
    FListbox2: TstringList;
    FListbox: TstringList;
    FTitle: string;
    procedure SetListbox(const Value: TstringList);
    procedure SetListbox2(const Value: TstringList);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FirstListbox: TstringList read FListbox write SetListbox;
    property SecondListbox: TstringList read FListbox2 write SetListbox2;
    property FirstCaption: string read FCaption write FCaption;
    property SecondCaption: string read FCaption2 write FCaption2;
    property Title: string read FTitle write FTitle;
    function Execute: boolean; override;
  end;

implementation

resourcestring
  RC_Column1Caption = 'First Column';
  RC_Column2Caption = 'Second Column';
  RC_FormCaption = 'Listbox Editor';

  {**************************************************}

constructor TJvExchListboxes.Create(AOwner: TComponent);
begin
  inherited;
  FListbox := TStringList.Create;
  FListbox2 := TStringList.Create;
  FCaption := RC_Column1Caption;
  FCaption2 := RC_Column2Caption;
  FTitle := RC_FormCaption;
end;
{**************************************************}

function TJvExchListboxes.Execute: boolean;
begin
  Result := TFormListb.Execute(FTitle,FCaption,FCaption2,FListBox,FListBox2);
end;
{**************************************************}

procedure TJvExchListboxes.SetListbox(const Value: TstringList);
begin
  FListbox.Assign(Value);
end;

procedure TJvExchListboxes.SetListbox2(const Value: TstringList);
begin
  FListbox2.Assign(Value);
end;

end.

