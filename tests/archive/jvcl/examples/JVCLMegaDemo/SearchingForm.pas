unit SearchingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvListView, Buttons, JvBitBtn, JclStrings,
  ArrowButtonMainFormU,JvCaptionPanel, JvLabel, JvBlinkingLabel;

type
  TSearchingFormMain = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ComponentEdit: TEdit;
    SearchButton: TJvBitBtn;
    Label3: TLabel;
    ResultListView: TJvListView;
    StatusLabel: TJvLabel;
    procedure SearchButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    TheForm: TForm;
    procedure CreateDemoForm(const ID: integer);
  public
    { Public declarations }
  end;

var
  SearchingFormMain: TSearchingFormMain;

  
implementation

uses UnitMain, JvOLBar,
  jvFrameEmpty, hello, JvLabelsU, JvFormsU, JvDialogsU, jvButtonsU, JvDateTimeU,
  JvPanelsU, JvChoosersU, JvUtilsU, JvControlsU, JvSearchFiles, JvWinDialogsU,
  JvEditsU, BmpAnimMainFormU, ChangeNotificationMainFormU, JvAniMainFormU,
  JvMousePositionnerMainFormU, MonthCalendarMainFormU, MailExampleMainFormU,
  OLBarMainFormU, JvSearchFileMainFormU, JvNTEventLogMainFormU, JvMruListMainFormU,
  JvLogFileMainFormU, InstallLabelMainFormU, JvAppHotKeyDemoMainFormU,
  JvDBDateTimePickerMainFormU, ContentScrollerMainFormU, JvDataEmbeddedMainFormU,
  JvBrowseFolderMainFormU, CreateProcessExampleMainFormU,
  JvClipboardViewerMainFormU, JvZoomMainFormU, JvSpecialProgressMainFormU,
  JvColorComboDemoMainFormU, JvInspectorDBDemoMainFormU, JvWindowsTitleMainFomU,
  RaHtHintsMainFormU, FileListBoxMainFormU, JvTreeViewAsMenuMainFormU,
  ListCombMainFormU, ControlsExampleMainFormU, JvBalloonHintMainFormU,
  DSADialogsMainFormU, MessageDlgEditorMain, JvHTMLParserMainFormU,
  JvLinkLabelMainFormU, JvScreenCaptureMainFormU, JvShellHookDemoMainFormU,
  JvShFileOperationMainFormU, JvSystemPopup2MainFormU, JvSystemPopupMainFormU,
  JvThumbnailMainFormU, JvTranslatorMainFormU, JvWndProcHookDemoMainFormU,
  RegTVMainFormU, RunDll32MainFormU, ScrollWinMainFormU, TimelineMainFormU,
  TipOfDayMainFormU, TMTimeLineMainFormU, TransBtnFormMainU,
  JvZLibMultipleMainFormU, OtherStandAlone, Profiler32MainFormU,
  FindReplaceMainFormU, JvPlayListMainFormU, ImageWindowMainFormU,
  RessourcesFormMain;

{$R *.dfm}

procedure TSearchingFormMain.CreateDemoForm(const ID: integer);
begin

  case ID of

     1 : TheForm  := TJvFormsFrm.Create(nil);
     2 : TheForm  := TJvDialogs.Create(nil);
     3 : TheForm  := TJvUtilsFrm.Create(nil);
     4 : TheForm  := TJvLabelsFrm.Create(nil);
     5 : TheForm := TRaHtHintsMainForm.Create(nil);
     6 : TheForm := TJvZoomMainForm.Create(nil);
     7 : TheForm := TJvEdits.Create(nil);
     8 : TheForm := TSearchingFormMain.Create(nil);
     9 : TheForm := TJvPanelsFrm.Create(nil);
    10 : TheForm := TMonthCalendarMainForm.Create(nil);
    11 : TheForm := TJvSearchFileMainForm.Create(nil);
    12 : TheForm := TJvDateTimeFrm.Create(nil);
    13 : TheForm := TJvChoosersFrm.Create(nil);
    14 : TheForm := TJvControls.Create(nil);
    15 : TheForm := TJvAniMainForm.Create(nil);
    16 : TheForm := TJvMousePositionnerMainForm.Create(nil);
    17 : TheForm := TJvDataEmbeddedMainForm.Create(nil);
    18 : TheForm := TBmpAnimMainForm.Create(nil);
    19 : TheForm := TArrowButtonMainForm.Create(nil);
    20 : TheForm := TJvClipboardViewerMainForm.Create(nil);
    21 : TheForm := TJvBrowseFolderMainForm.Create(nil);
    22 : TheForm := TInstallLabelMainForm.Create(nil);
    24 : TheForm := TJvLogFileMainForm.Create(nil);
    25 : TheForm := TOLBarMainForm.Create(nil);
    26 : TheForm  := TControlsExampleMainForm.Create(nil);
    27 : TheForm := TChangeNotificationMainForm.Create(nil);
    28 : TheForm := TCreateProcessExampleMainForm.Create(nil);
    29 : TheForm := TJvNTEventLogMainForm.Create(nil);
    30 : TheForm := TJvAppHotKeyDemoMainForm.Create(nil);
    31 : TheForm := TJvWindowsTitleMainForm.Create(nil);
    32 : TheForm := TJvSpecialProgressMainForm.Create(nil);
    33 : TheForm := TJvColorComboDemoMainForm.Create(nil);
    34 : TheForm := TContentScrollerMainForm.Create(nil);
    35 : TheForm := TMailExampleMainForm.Create(nil);
    36 : TheForm := TJvTreeViewAsMenuMainForm.Create(nil);
    37 : TheForm := TListCombMainForm.Create(nil);
    38 : TheForm := TJvDBDateTimePickerMainForm.Create(nil);
    39 : TheForm := TJvInspectorDBDemoMainForm.Create(nil);
    40 : TheForm := TJvMruListMainForm.Create(nil);
    41 : TheForm := TFileListBoxMainForm.Create(nil);
    42 : TheForm := TJvButtons.Create(nil);
    43 : TheForm := TJvBalloonHintMainForm.Create(nil);
    44 : TheForm := TDSADialogsMainForm.Create(nil);
    45 : TheForm := TfrmMessageDlgEditor.Create(nil);
    46 : TheForm := TJvHTMLParserMainForm.Create(nil);
    47 : TheForm := TJvLinkLabelMainForm.Create(nil);
    48 : TheForm := TJvScreenCaptureMainForm.Create(nil);
    49 : TheForm := TJvShellHookDemoMainForm.Create(nil);
    50 : TheForm := TJvShFileOperationMainForm.Create(nil);
    51 : TheForm := TJvSystemPopupMainForm.Create(nil);
    52 : TheForm := TJvSystemPopup2MainForm.Create(nil);
    53 : TheForm := TJvThumbnailMainForm.Create(nil);
    54 : TheForm := TJvTranslatorMainForm.Create(nil);
    55 : TheForm := TJvWndProcHookDemoMainForm.Create(nil);
    56 : TheForm := TJvWndProcHookDemoMainForm.Create(nil);
    57 : TheForm := TRegTVMainForm.Create(nil);
    58 : TheForm := TRunDll32MainForm.Create(nil);
    59 : TheForm := TJvScrollingWindowMainForm.Create(nil);
    60 : TheForm := TTimelineMainForm.Create(nil);
    61 : TheForm := TTipOfDayMainForm.Create(nil);
    62 : TheForm := TTMTimeLineMainForm.Create(nil);
    63 : TheForm := TTransBtnFormMain.Create(nil);
    64 : TheForm := TJvZLibMultipleMainForm.Create(nil);
    65 : TheForm  := TWelcomeForm.Create(nil);
    66 : TheForm := TOtherMainForm.Create(nil);
    67 : TheForm := TProfiler32MainForm.Create(nil);
    68 : TheForm := TFindReplaceMainForm.Create(nil);
    69 : TheForm := TJvPlaylistMainForm.Create(nil);
    70 : TheForm := TImageWindowMainForm.Create(nil);
    71 : TheForm := TRessourcesForm.Create(nil);
  else
    TheForm := tfrEmpty.create(nil);
  end;


end;


procedure TSearchingFormMain.SearchButtonClick(Sender: TObject);
var
  I,J,K: integer;
  ListItem: TListItem;
begin
  try
    Screen.Cursor := crHourglass;
    StatusLabel.Visible := true;
    Application.ProcessMessages;
    ResultListView.Items.Clear;
    for I := 0 to Mainform.JvOutlookBar1.Pages.Count - 1 do
      for J := 0 to Mainform.JvOutlookBar1.Pages[I].Buttons.Count - 1 do
      begin
        try
          Self.CreateDemoForm(Mainform.JvOutlookBar1.Pages[I].Buttons[J].Tag);
        except
        end;
        //find occurence of component
        for K := 0 to TheForm.ComponentCount -1 do
          if StrIPos(ComponentEdit.Text,TheForm.Components[K].ClassName) > 0 then
          begin
            //Found! - Inserting into ListView
            ListItem := ResultListView.Items.Add;
            ListItem.Caption := TheForm.Name;
            Application.ProcessMessages;
            //ListItem.SubItems.Add(ExtractFilePath(AName));
            break;
          end;
        if (TheForm <> nil) then
          freeAndNil(TheForm);
      end;
  finally
    Screen.Cursor := crDefault;
    StatusLabel.Visible := false;
    if (TheForm <> nil) then
      freeAndNil(TheForm);
  end;
end;

procedure TSearchingFormMain.FormCreate(Sender: TObject);
begin
  StatusLabel.Visible := false;
end;

end.
