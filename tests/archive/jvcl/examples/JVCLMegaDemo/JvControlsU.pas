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
  TJvControls = class(TForm)
    JvGradientCaption1: TJvGradientCaption;
    JvWaitingGradient1: TJvWaitingGradient;
    Label3: TLabel;
    Label4: TLabel;
    ImageList1: TImageList;
    JvImageListBox1: TJvImageListBox;
    JvMultilineListbox1: TJvMultilineListbox;
    JvTimeLine1: TJvTimeLine;
  end;

implementation

{$R *.DFM}

end.
