{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvButtonShaped.PAS, released on 2001-02-28.

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

unit JvButtonShaped;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvButton;

type
  TJvButtonShaped = class(TJvButton)
  private
    FWorking: Boolean;
    FShape: TBitmap;
    procedure SetShape(const Value: TBitmap);
  protected
    procedure Loaded;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Shape: TBitmap read FShape write SetShape;
  end;

implementation

uses
  JvFunctions;

constructor TJvButtonShaped.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShape := TBitmap.Create;
  FWorking := False;
end;

destructor TJvButtonShaped.Destroy;
begin
  FShape.Free;
  inherited Destroy;
end;

procedure TJvButtonShaped.Loaded;
begin
  inherited;
  if not FShape.Empty then
    SetShape(Shape);
end;

procedure TJvButtonShaped.SetShape(const Value: TBitmap);
begin
  FShape.Assign(Value);
  if not FWorking then
  try
    FWorking := True;
    SetWindowRgn(Handle, RegionFromBitmap(Value), True);
  finally
    FWorking := False;
  end;
  FWorking := False;
end;

end.

