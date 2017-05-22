unit MDIBkgndDemoChld;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBackgrounds, MDIBkgndDemoMain, Buttons;

type
  TMDIChildForm = class(TForm)
    BackgroundSwitch: TSpeedButton;
    procedure BackgroundSwitchClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MDIChildForm: TMDIChildForm;

implementation

{$R *.DFM}

procedure TMDIChildForm.BackgroundSwitchClick(Sender: TObject);
begin
  if BackgroundSwitch.Down then
    MDIMainForm.Background.Clients.Add(Self)
  else
    MDIMainForm.Background.Clients.Remove(Self);
end;

end.
