unit JvPanelsU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvTransBtn, StdCtrls, ExtCtrls, JvSizeablePanel,
  JvItemsPanel, JvOLBar, JvClock, JvRollOut, JvComponent, JvCaptionPanel,
  JvLookOut, ImgList, JvAnalogClock, JvPanel;

type
  TJvPanelsFrm = class(TForm)
    ImageList1: TImageList;
    JvLookOut1: TJvLookOut;
    JvLookOutButton1: TJvLookOutButton;
    JvCaptionPanel1: TJvCaptionPanel;
    JvRollout1: TJvRollout;
    JvClock1: TJvClock;
    JvAnalogClock1: TJvAnalogClock;
    JvExpress1: TJvExpress;
    JvExpressButton1: TJvExpressButton;
    JvSizeablePanel1: TJvSizeablePanel;
    JvItemsPanel1: TJvItemsPanel;
    JvOutlookBar1: TJvOutlookBar;
  end;

implementation

{$R *.DFM}

end.
