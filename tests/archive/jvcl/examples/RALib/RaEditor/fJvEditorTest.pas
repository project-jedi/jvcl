unit fJvEditorTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvEditor, JvHLParser, StdCtrls, ExtCtrls, ComCtrls, JvRegAuto, JvHLEditor
  {$IFDEF COMPILER4_UP}, ImgList, JvComponent {$ENDIF COMPILER4_UP};

type
  TfrmEditor  = class(TForm)
    GutterImages: TImageList;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    JvEditor: TJvHLEditor;
    TabSheet2: TTabSheet;
    RAEditor1: TJvHLEditor;
    TabSheet3: TTabSheet;
    RAEditor2: TJvHLEditor;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    RAEditor3: TJvHLEditor;
    RAEditor4: TJvEditor;
    Panel1: TPanel;
    Label1: TLabel;
    ilCompletions: TImageList;
    RegAuto1: TJvRegAuto;
    TabSheet6: TTabSheet;
    RAHLEditor1: TJvHLEditor;
    TabSheet7: TTabSheet;
    RAHLEditor2: TJvHLEditor;
    TabSheet8: TTabSheet;
    RAHLEditor3: TJvHLEditor;
    TabSheet9: TTabSheet;
    RAHLEditor4: TJvHLEditor;
    StatusBar1: TStatusBar;
    TabSheet10: TTabSheet;
    RAHLEditor5: TJvHLEditor;
    TabSheet11: TTabSheet;
    RAHLEditor6: TJvHLEditor;
    TabSheet12: TTabSheet;
    RAHLEditor7: TJvHLEditor;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RAEditorPaintGutter(Sender: TObject; Canvas: TCanvas);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Enter(Sender: TObject);
    procedure RAEditorCompletionDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure RegAuto1AfterLoad(Sender: TObject);
    procedure RegAuto1AfterSave(Sender: TObject);
    procedure RAEditor3ReservedWord(Sender: TObject; Token: String;
      var Reserved: Boolean);
    procedure RAEditorCompletionApply(Sender: TObject;
      const OldString: String; var NewString: String);
  private
    Parser : TJvIParser;
  end;

var
  frmEditor: TfrmEditor ;

implementation

uses JvStrUtil, JvCtlConst;

{$R *.DFM}

procedure TfrmEditor .FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  Parser := TJvIParser.Create;
end;

procedure TfrmEditor .FormDestroy(Sender: TObject);
begin
  Parser.Free;
end;

procedure TfrmEditor .RAEditorPaintGutter(Sender: TObject; Canvas: TCanvas);
  procedure Draw(Y, ImageIndex : integer);
  var
    Ro : integer;
    R : TRect;
  begin
    if Y <> -1 then
      with Sender as TJvEditor do begin
        Ro := Y - TopRow;
        R := CalcCellRect(0, Ro);
        GutterImages.Draw(Canvas,
          GutterWidth -GutterRightMargin -GutterImages.Width{R.Left},
          R.Top + (CellRect.Height - GutterImages.Height) div 2 +1,
          ImageIndex);
      end;
  end;
var
  i  : integer;
begin
  for i := 0 to 9 do
    if (Sender as TJvEditor).Bookmarks[i].Valid then
      Draw((Sender as TJvEditor).Bookmarks[i].Y, i);
end;

procedure TfrmEditor .PageControl1Change(Sender: TObject);
begin
  Parser.Style := TIParserStyle(not(PageControl1.ActivePage.PageIndex=0));
  JvEditor.Refresh;
  RAEditor1.Refresh;
  RAEditor2.Refresh;
  RAEditor3.Refresh;
end;

procedure TfrmEditor .PageControl1Enter(Sender: TObject);
begin
  (PageControl1.ActivePage.Controls[0] as TWinControl).SetFocus;
end;

procedure TfrmEditor .RAEditorCompletionDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
	Offset, W : Integer;
  S : string;
  ImageIndex : integer;
begin
  Offset := 3;
  with Control as TListBox, (Control.Owner as TJvEditor).Completion do
  begin
    Canvas.FillRect(Rect);
    case Mode of
      cmIdentifiers:
        begin
          ImageIndex := StrToInt(Trim(SubStr(Items[Index], 2, Separator))) - 1;
          ilCompletions.Draw(Canvas, Rect.Left + 2, Rect.Top, ImageIndex);
          Canvas.TextOut(Rect.Left + 3*Offset + ilCompletions.Width, Rect.Top +2, SubStr(Items[Index], 0, Separator));
          S := Trim(SubStr(Items[Index], 1, Separator));
          W := Canvas.TextWidth(S);
          Canvas.TextOut(Rect.Right - 2*Offset - W, Rect.Top +2, S);
        end;
      cmTemplates:
        begin
          Canvas.TextOut(Rect.Left + Offset, Rect.Top +2, SubStr(Items[Index], 1, Separator));
          Canvas.Font.Style := [fsBold];
          S := SubStr(Items[Index], 0, Separator);
          W := Canvas.TextWidth(S);
          Canvas.TextOut(Rect.Right - 2*Offset - W, Rect.Top +2, S);
        end;
    end;
  end;  
end;

procedure TfrmEditor .RegAuto1AfterLoad(Sender: TObject);
begin
  PageControl1.ActivePage := PageControl1.Pages[RegAuto1.ReadInteger(Name, 'PageIndex', PageControl1.ActivePage.PageIndex)];
  PageControl1Change(nil);
end;


procedure TfrmEditor .RegAuto1AfterSave(Sender: TObject);
begin
  RegAuto1.WriteInteger(Name, 'PageIndex', PageControl1.ActivePage.PageIndex);
end;

procedure TfrmEditor .RAEditor3ReservedWord(Sender: TObject; Token: String;
  var Reserved: Boolean);
begin
  Reserved := (Token = 'R') or (Token = '&') or (Token = 'A') or
    Cmp(Token, 'Library');
end;

procedure TfrmEditor .RAEditorCompletionApply(Sender: TObject;
  const OldString: String; var NewString: String);
begin
  NewString := OldString + Copy(NewString, Length(OldString) + 1, 10000);
end;

end.
