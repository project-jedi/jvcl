{$I JVCL.INC}

unit JvCoreReg;

interface

procedure Register;

implementation
uses
  Classes, Controls, StdCtrls, ExtCtrls, Graphics, ActnList, ImgList, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvJCLUtils, JVCLVer, JvComponent, JvActions, JvActnResForm, JvJVCLAboutForm, JvDsgnEditors, JvIDEZoom,
  JvJVCLAboutEditor, JvBaseDlgEditor, JvColorEditor, JvPaintBoxEditor, JvContextProvider;

{$R ..\resources\JvCoreReg.dcr}

procedure Register;
const
 BaseClass:TClass = TComponent;

begin
  RegisterComponents(SPaletteNonVisual,[
    TJvJVCLAboutComponent,
    TJvContextProvider
    ]);

  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL', TJVCLAboutDialogProperty);
  {.$DEFINE JVCL_REGISTER_GLOBAL_DESIGNEDITORS} // - just for testing
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
    RegisterPropertyEditor(TypeInfo(TDate), nil, '', TJvDateExProperty);
    RegisterPropertyEditor(TypeInfo(TTime), nil, '', TJvTimeExProperty);
    RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TJvDateTimeExProperty);
    RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TJvColorProperty);
    RegisterPropertyEditor(TypeInfo(string), BaseClass, 'FolderName', TJvDirectoryProperty);
    RegisterPropertyEditor(TypeInfo(string), BaseClass, 'DirectoryName', TJvDirectoryProperty);
    RegisterPropertyEditor(TypeInfo(string), BaseClass, 'Hint', TJvHintProperty);
    RegisterPropertyEditor(TypeInfo(TCaption), BaseClass, '', TJvHintProperty);


    RegisterPropertyEditor(TypeInfo(Integer), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(ShortInt), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(SmallInt), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Longint), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Word), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Byte), BaseClass, '', TJvIntegerProperty);
    RegisterPropertyEditor(TypeInfo(Cardinal), BaseClass, '', TJvIntegerProperty);

    RegisterPropertyEditor(TypeInfo(Single), BaseClass, '', TJvFloatProperty);
    RegisterPropertyEditor(TypeInfo(Double), BaseClass, '', TJvFloatProperty);
    RegisterPropertyEditor(TypeInfo(Extended), BaseClass, '', TJvFloatProperty);
    RegisterPropertyEditor(TypeInfo(Currency), BaseClass, '', TJvFloatProperty);
    RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TJvPictProperty);
    RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TJvGraphicPropertyEditor);

    {$IFNDEF DelphiPersonalEdition}
      RegisterComponentEditor(TPaintBox, TJvPaintBoxEditor);
      RegisterComponentEditor(TCustomImageList, TJvImageListEditor);
      RegisterComponentEditor(TImageList, TJvImageListEditor);
    {$ENDIF}
    RegisterComponentEditor(TImage, TJvGraphicsEditor);
    RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TShortCut), TJvComponent, '', TJvShortCutProperty);
  RegisterPropertyEditor(TypeInfo(TDayOfWeekName), nil, '', TJvWeekDayProperty);

  RegisterActions('JVCL', [TJvSendMail, TJvWebAction], TJvStandardActions);
  RegisterZoom;
  
end;

end.
