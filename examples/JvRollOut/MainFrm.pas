unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExExtCtrls, JvComponent, JvRollOut, ImgList,
  StdCtrls;

type
  TForm1 = class(TForm)
    Splitter1: TSplitter;
    ImageList1: TImageList;
    Panel4: TPanel;
    chkShowFocus: TCheckBox;
    chkTabStop: TCheckBox;
    chkToggleAnywhere: TCheckBox;
    Panel3: TPanel;
    pnlRightAlign: TPanel;
    RO40: TJvRollOut;
    RO39: TJvRollOut;
    RO38: TJvRollOut;
    RO37: TJvRollOut;
    RO36: TJvRollOut;
    RO35: TJvRollOut;
    RO34: TJvRollOut;
    RO33: TJvRollOut;
    RO32: TJvRollOut;
    RO31: TJvRollOut;
    pnlLeftAlign: TPanel;
    RO30: TJvRollOut;
    RO29: TJvRollOut;
    RO28: TJvRollOut;
    RO27: TJvRollOut;
    RO26: TJvRollOut;
    RO25: TJvRollOut;
    RO24: TJvRollOut;
    RO23: TJvRollOut;
    RO22: TJvRollOut;
    RO21: TJvRollOut;
    Panel1: TPanel;
    pnlTopAlign: TPanel;
    RO1: TJvRollOut;
    RO2: TJvRollOut;
    RO3: TJvRollOut;
    RO4: TJvRollOut;
    RO5: TJvRollOut;
    RO6: TJvRollOut;
    RO7: TJvRollOut;
    RO8: TJvRollOut;
    RO9: TJvRollOut;
    RO10: TJvRollOut;
    pnlBottomAlign: TPanel;
    RO20: TJvRollOut;
    RO19: TJvRollOut;
    RO18: TJvRollOut;
    RO17: TJvRollOut;
    RO16: TJvRollOut;
    RO15: TJvRollOut;
    RO14: TJvRollOut;
    RO13: TJvRollOut;
    RO12: TJvRollOut;
    RO11: TJvRollOut;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    chkGroupIndex: TCheckBox;
    procedure chkShowFocusClick(Sender: TObject);
    procedure chkTabStopClick(Sender: TObject);
    procedure chkToggleAnywhereClick(Sender: TObject);
    procedure chkGroupIndexClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.chkShowFocusClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
      TJvRollOut(Components[i]).ShowFocus := chkShowFocus.Checked;
end;

procedure TForm1.chkTabStopClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
      TJvRollOut(Components[i]).TabStop   := chkTabStop.Checked;
end;

procedure TForm1.chkToggleAnywhereClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
      TJvRollOut(Components[i]).ToggleAnywhere := chkToggleAnywhere.Checked;
end;

procedure TForm1.chkGroupIndexClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
      TJvRollOut(Components[i]).GroupIndex := Ord(chkGroupIndex.Checked);
end;

end.
