unit JvControlsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvButton, JvButtonShaped, JvFavoritesButton, JvStartMenuBtn,
  JvClock, JvCombobox, JvGammaPanel, JvGradientCaption,
  Spin, JvZoom, JvWaitingGradient, JvControlPanel, JvRecentMenuBtn,
  ExtCtrls, JvShape, JvMultilineListbox, JvListComb, ImgList, JvComponent,
  JvTimeLine;

type
  TJvControls = class(TFrame)
    JvGradientCaption1: TJvGradientCaption;
    JvWaitingGradient1: TJvWaitingGradient;
    Label3: TLabel;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    Label5: TLabel;
    JvZoom1: TJvZoom;
    JvShape1: TJvShape;
    ImageList1: TImageList;
    JvImageListBox1: TJvImageListBox;
    JvMultilineListbox1: TJvMultilineListbox;
    JvTimeLine1: TJvTimeLine;
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

procedure TJvControls.SpinEdit1Change(Sender: TObject);
begin
 JvZoom1.ZoomLevel := SpinEdit1.Value;
end;

end.
