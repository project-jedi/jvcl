unit FavoritesFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls, ImgList;

type
  TFavoritesForm = class(TForm)
    lbDockClient1: TJvDockClient;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ImageList1: TImageList;
    ListBox1: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FavoritesForm: TFavoritesForm;

implementation

uses MSDN2002MainUnit;

{$R *.DFM}

end.
