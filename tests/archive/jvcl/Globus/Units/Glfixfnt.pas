unit glFixFnt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TPublicControlFont = class(TControl)
  public
    property Font;
  end;

  TglFixFont = class(TComponent)
  private
    procedure FixFont( Window: TWinControl );
  protected
    { Protected declarations }
  public
    constructor Create( AOwner: TComponent ); override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Proba', [TglFixFont]);
end;
//____________________________
constructor TglFixFont.Create( AOwner: TComponent );
begin
  inherited;
  FixFont( TWinControl(Owner) );
end;

procedure TglFixFont.FixFont( Window: TWinControl );
var i: integer;
begin
  with Window do
  begin
    TPublicControlFont( Window ).Font.Size := 8;
    for i:=0 to ComponentCount-1 do
    if Components[i] is TWinControl then FixFont( TWinControl(Components[i]) )
    else
      if Components[i] is TControl then
	TPublicControlFont(Components[i]).Font.Size := 8;
  end;
end;

end.
