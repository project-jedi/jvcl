{
  
  							          
 	      Freeware Delphi VCL Extensions Library	           
 		          ' GLOBUS LIB '		           
  		                                                  
               Copyright (c) 1998-2002 Chudin A.V	          
  	   chudin@biblio-globus.ru; avchudin@yandex.ru            
  
  this unit contains registration procedures for Delphi 4 - 7
}
unit reggllib_delphi;

{$I glDEF.INC}

interface

//{$DEFINE INC_ALPHA_UNITS} // компилировать также недоработанные компоненты

procedure Register;

implementation

uses Classes,

     { beta version units - готовые к использованию компоненты }
     glBevel, glLabel, glEdit, glCBox, glTView, glFlyTxt, glPage, glTab,
     glHint, gl3DCol,glCapt,glprgrs, glHShape, glJump,
     glDigits, glGrBox, glImage, glShadow, glLBox, glALBox, glSBox, glGBtn, glRLabel,
     glBitBtn, glRuler, glSGrid,glProcs, glSysInf, glSplit, {glShape,}
     {glReport, geReport,} glMSlots, glExceptionHandler, glSpeedButton,
     glSingleInstance, glHelpPanel, glStringContainer, glSysRequirements,
     glSmallFontsDefence, glWizardHeader, {$IFDEF GLVER_D5} glXMLSerializer, glLanguageLoader, {$ENDIF}
     glExportComponents, geShadow, geHelpPanel

     { alpha version units - компоненты в стадии доработки }
     {$IFDEF INC_ALPHA_UNITS}
     {glMRes, glButton,  },
     glPropCn,
     glGHC,
     GLCTABLE,
     geRPForm,
     geCList,
     geGHC,
     glLogics, geLogics,
     glinspectorGrid
     {$ENDIF}
     {$IFDEF GLVER_D6}, DesignIntf, DesignWindows, DesignEditors{$ELSE} {$IFDEF GLVER_D4}, dsgnintf{$ENDIF} {$ENDIF};

procedure Register;
begin
  RegisterComponents( 'Gl Controls', [ TglBevel, TglLabel, TglBitBtn, TglGraphicButton,TglMaskEdit ,TglCheckBox, TglTreeView, TglCheckTreeView, TglFlyingText,
                                       TglPageControl, TglTabControl, TglProgress, TglHoleShape,
                                       TglDigits, TglShadow, TglGroupBox, TglBitmapImage, TglStaticText, TglListBox,
                                       TglCheckListBox, TglAskListBox, TglScrollBox, TglMaskEdit, TglRuler, TglStringGrid,
                                       TglSplitter, TglSpeedButton,
                                       TglHelpPanel, TglWizardHeader
                                       ]);

  RegisterComponents( 'Gl Components', [ Tgl3DColors, TglCaption, TglHint, TglProcess, TglSysInfo,
                                        TglJumpingComponent, TglMailSlotServer, TglMailSlotClient,
                                        //TglReport, TglReportEditor,
                                        TglExceptionHandler, TglSingleInstance, TglStringContainer, TglSysRequirements,
                                        TglSmallFontsDefence {, TglMultiResources}]);
  {$IFDEF GLVER_D5}
  RegisterComponents( 'Gl Components', [ TglXMLSerializer, TglLanguageLoader ]);
  {$ENDIF}

  RegisterComponents( 'Gl QReport', [ TglQRLabel, TglQRDBText ] );

  RegisterComponents('Gl ExportImport', [TglExportExcel, TglExportDBETable{, TglExportHTML, TglExportXML}]);

  {$IFDEF INC_ALPHA_UNITS}
  RegisterComponents( 'Gl Controls', [ TglGridHeaderControl, TglinspectorGrid {, TglButton} ]);
  RegisterComponents( 'Gl Components', [ TglReportParamsEditor, TLogicProducer {, TglMultiResources} ]);
  RegisterComponents( 'Gl DB', [ TglPrintCrossTable ] );

  RegisterComponentEditor(TglPropertyCenter, TglComponentListEditor);
  RegisterPropertyEditor( TypeInfo(TStringList), TglPropertyCenter, 'ComponentList', TglComponentListProperty );
  RegisterComponentEditor(TglReportParamsEditor, TglRepParams_Editor);
  RegisterComponentEditor(TglGridHeaderControl, TglGridHeaderControl_Editor);
  RegisterComponentEditor(TLogicProducer, TglLogicsComponentEditor);
{
  RegisterComponents( 'Gl Components', [ Tgl3DColors, TglCaption,
		      TglHint, TglJumpingComponent, TglProcess, TglStringContainer,
		      TglMultiResources , TglPropertyCenter, TglSysInfo,
                      TglReport, TglReportEditor, TglReportParamsEditor] );
  RegisterComponents( 'Gl DBAware', [ TglDBGrid, TglVertDBSGrid, TglPrintCrossTable ] );
}
//  RegisterPropertyEditor(TypeInfo(string), TglEdit, 'EditMask', TMaskProperty);
//  RegisterPropertyEditor(TypeInfo(string), TglProcess, 'FileName', TFilenameProperty);
//  RegisterPropertyEditor( TypeInfo(TResStringList), TglMultiResources, 'Resources', TglResourcesProperty );
  {$ENDIF};


//  RegisterComponentEditor(TglReport, TglRep_Editor);
//  RegisterPropertyEditor( TypeInfo(TStringList), TglReport, 'Report', TglRep_Property );
//  RegisterComponentEditor(TglReportEditor, TglRep_Editor);


  RegisterComponentEditor(TglShadow, TglShadow_Editor);
  RegisterComponentEditor(TglHelpPanel, TglHelpPanel_Editor);

end;

end.

