{$I JVCL.INC}

unit JvCustomReg;

interface

procedure Register;

implementation
uses
  Classes, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  FiltEdit, ExptIntf, ToolsApi,
  JclSchedule,

  JvConsts, JvTrayIcon, JvGammaPanel, JvInspector, JvLinkLabel,
  JvLookOut, JvOutlookBar, JvScheduledEvents, JvThumbImage,
  JvThumbnails, JvThumbviews, JvTimeLine, JvTMTimeLine, JvBalloonHint,
  JvValidateEdit, JvEditor, JvHLEditor, JvHLEditorPropertyForm, JvHLParser,
  JvTimeLineEditor, JvHLEditEditor, JvScheduleEditors,
  JvOutlookBarEditors, JvLookoutEditor;


{$R ..\resources\JvCustomReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteButton,[
    TJvLookOutButton, TJvExpressButton
    ]);
  RegisterComponents(SPaletteEdit,[
    TJvValidateEdit,TJvEditor, TJvHLEditor, TJvHLEdPropDlg
    ]);
  RegisterComponents(SPaletteBarPanel,[
    TJvGammaPanel,TJvOutlookBar, TJvLookout, {TJvLookOutPage, } TJvExpress
    ]);
  RegisterComponents(SPaletteLabel,[
    TJvLinkLabel
    ]);
  RegisterComponents(SPaletteImageAnimator,[
    TJvThumbView, TJvThumbnail, TJvThumbImage
    ]);
  RegisterComponents(SPaletteVisual,[
    TJvInspector, TJvInspectorBorlandPainter, TJvInspectorDotNETPainter, TJvTimeLine, TJvTMTimeLine
    ]);
  RegisterComponents(SPaletteNonVisual,[
    TJvTrayIcon, TJvScheduledEvents, TJvBalloonHint
    ]);

  RegisterPropertyEditor(TypeInfo(Integer),
    TJvCustomOutlookBar, 'ActivePageIndex', TJvOutlookBarActivePageEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages),
    TJvCustomOutlookBar, '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarButtons),
    TJvOutlookBarPage, '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer),
    TJvOutlookBarButton, 'ImageIndex', TJvOutlookBarButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TJvColors),
    TJvHLEditor, 'Colors', TJvHLEditorColorProperty);
  RegisterPropertyEditor(TypeInfo(IJclSchedule), TJvEventCollectionItem, 'Schedule', TJvSchedulePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvLookoutButton,
    'ImageIndex', TJvLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvExpressButton,
    'ImageIndex', TJvLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvThumbView,
    'Filter', TFilterProperty);

  RegisterComponentEditor(TJvHLEdPropDlg, TJvHLEdPropDlgEditor);
  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarComponentEditor);
  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);
  RegisterComponentEditor(TJvLookOut, TJvLookOutEditor);
  RegisterComponentEditor(TJvLookOutPage, TJvLookOutPageEditor);
  RegisterComponentEditor(TJvExpress, TJvExpressEditor);
  RegisterComponentEditor(TJvCustomScheduledEvents, TJvSchedEventComponentEditor);
  RegisterClass(TJvLookoutPage);
end;

end.
