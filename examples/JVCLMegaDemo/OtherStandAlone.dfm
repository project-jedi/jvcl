object OtherMainForm: TOtherMainForm
  Left = 345
  Top = 87
  Width = 745
  Height = 555
  Caption = 'other Demos'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvHotLink1: TJvLabel
    Left = 24
    Top = 48
    Width = 288
    Height = 16
    Cursor = crHandPoint
    Caption = 'call the batch file to build all JVCL examples now!'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkBuildJVCLClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = ' '
  end
  object Label1: TLabel
    Left = 23
    Top = 12
    Width = 664
    Height = 32
    Caption = 
      'Some of the included demos are not suitable for integrating them' +
      ' in this MegaDemo. So I decided to link'#13#10'to their exe files from' +
      ' here. But first you have to build all demos from the JVCL with ' +
      'the batch below.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object JvHotLink2: TJvLabel
    Left = 34
    Top = 380
    Width = 129
    Height = 16
    Cursor = crHandPoint
    Caption = 'JvCaptionButtonDemo'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'CaptionButtonDemo.exe'
  end
  object JvHotLink4: TJvLabel
    Left = 34
    Top = 400
    Width = 94
    Height = 16
    Cursor = crHandPoint
    Caption = 'EnvironmentList'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'EnvironmentList.exe'
  end
  object JvHotLink3: TJvLabel
    Left = 34
    Top = 420
    Width = 96
    Height = 16
    Cursor = crHandPoint
    Caption = 'JvTrayIconDemo'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'JvTrayIconDemo.exe'
  end
  object JvHotLink5: TJvLabel
    Left = 34
    Top = 81
    Width = 252
    Height = 16
    Cursor = crHandPoint
    Caption = 'RAControls (also some kind of MegaDemo)'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'RAControls.exe'
  end
  object JvHotLink6: TJvLabel
    Left = 34
    Top = 100
    Width = 50
    Height = 16
    Cursor = crHandPoint
    Caption = 'DBMove'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'DBMove.exe'
  end
  object JvHotLink7: TJvLabel
    Left = 34
    Top = 131
    Width = 227
    Height = 16
    Cursor = crHandPoint
    Caption = 'how to draw  line numbers in a JvEditor'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'LineNumbers.exe'
  end
  object JvHotLink8: TJvLabel
    Left = 34
    Top = 151
    Width = 58
    Height = 16
    Cursor = crHandPoint
    Caption = 'RA Editor'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'RAEditorTest.exe'
  end
  object JvHotLink9: TJvLabel
    Left = 34
    Top = 227
    Width = 70
    Height = 16
    Cursor = crHandPoint
    Caption = 'RAHLEditor'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'RAHLEdPropDlgTest.exe'
  end
  object JvHotLink10: TJvLabel
    Left = 34
    Top = 259
    Width = 100
    Height = 16
    Cursor = crHandPoint
    Caption = 'JvInterpreter Test'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'JvInterpreterTest.exe'
  end
  object Label2: TLabel
    Left = 20
    Top = 63
    Width = 134
    Height = 16
    Caption = 'formerly RaLib stuff'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object JvHotLink11: TJvLabel
    Left = 34
    Top = 279
    Width = 121
    Height = 16
    Cursor = crHandPoint
    Caption = 'JvInterpreterEndUser'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'JvInterpreterEndUser.exe'
  end
  object JvHotLink12: TJvLabel
    Left = 34
    Top = 299
    Width = 52
    Height = 16
    Cursor = crHandPoint
    Caption = 'MDI App'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'MDIApp.exe'
  end
  object JvHotLink13: TJvLabel
    Left = 34
    Top = 319
    Width = 72
    Height = 16
    Cursor = crHandPoint
    Caption = 'RA Notepad'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'RANotepad.exe'
  end
  object Label3: TLabel
    Left = 20
    Top = 207
    Width = 209
    Height = 16
    Caption = 'formerly RaLib Interpreter stuff'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 20
    Top = 362
    Width = 108
    Height = 16
    Caption = 'other JVCL stuff'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 371
    Top = 155
    Width = 111
    Height = 16
    Caption = 'formerly Rx stuff'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object JvHotLink14: TJvLabel
    Left = 395
    Top = 173
    Width = 171
    Height = 16
    Cursor = crHandPoint
    Caption = 'RxDemo (their MegaDemo :-)'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'RxDemo.exe'
  end
  object JvHotLink15: TJvLabel
    Left = 395
    Top = 192
    Width = 165
    Height = 16
    Cursor = crHandPoint
    Caption = 'RxGIFAnim (now integrated)'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'RxGIFAnm.exe'
  end
  object JvHotLink16: TJvLabel
    Left = 395
    Top = 211
    Width = 72
    Height = 16
    Cursor = crHandPoint
    Caption = 'DB Explorer'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'DBEXPL32.exe'
  end
  object Label6: TLabel
    Left = 371
    Top = 236
    Width = 328
    Height = 16
    Caption = 'demos using Jim Cooper'#39's diagramming comps'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object JvHotLink17: TJvLabel
    Left = 395
    Top = 253
    Width = 72
    Height = 16
    Cursor = crHandPoint
    Caption = 'WebMapper'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'WebMapper.exe'
  end
  object JvHotLink18: TJvLabel
    Left = 395
    Top = 270
    Width = 55
    Height = 16
    Cursor = crHandPoint
    Caption = 'UseCase'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'UseCase.exe'
  end
  object JvHotLink19: TJvLabel
    Left = 395
    Top = 288
    Width = 115
    Height = 16
    Cursor = crHandPoint
    Caption = 'DependencyWalker'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'DepWalk.exe'
  end
  object Label7: TLabel
    Left = 371
    Top = 316
    Width = 281
    Height = 16
    Caption = 'there are also 2 articles from Jim Cooper'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 371
    Top = 380
    Width = 299
    Height = 16
    Caption = '3 HID Demos for the JvHidDeviceController'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object JvHotLink22: TJvLabel
    Left = 395
    Top = 433
    Width = 97
    Height = 16
    Cursor = crHandPoint
    Caption = 'ReadWriteDemo'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'SimpleHIDWrite.exe'
  end
  object JvHotLink23: TJvLabel
    Left = 395
    Top = 398
    Width = 69
    Height = 16
    Cursor = crHandPoint
    Caption = 'BasicDemo'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'BasicDemo.exe'
  end
  object JvHotLink24: TJvLabel
    Left = 395
    Top = 415
    Width = 93
    Height = 16
    Cursor = crHandPoint
    Caption = 'CollectionDemo'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnClick = JvHotLinkClick
    OnMouseEnter = JvHotLinkMouseEnter
    OnMouseLeave = JvHotLinkMouseLeave
    AutoOpenURL = False
    HotTrack = True
    HotTrackFont.Charset = ANSI_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = [fsUnderline]
    URL = 'CollectionDemo.exe'
  end
  object Label9: TLabel
    Left = 52
    Top = 168
    Width = 228
    Height = 26
    Caption = 
      'showing syntax highliging (JvHLEditor) and code'#13#10'completion for ' +
      'various languages (TJvIParser)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label10: TLabel
    Left = 52
    Top = 116
    Width = 167
    Height = 13
    Caption = 'uses JvaSQLScript and JvDBMove'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label11: TLabel
    Left = 52
    Top = 338
    Width = 268
    Height = 13
    Caption = 'Notepad with syntaxhighlighting (TJvEditor, TJvHLEditor)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label12: TLabel
    Left = 52
    Top = 243
    Width = 184
    Height = 13
    Caption = 'uses JvHLEdPropDlg and TJvHLEditor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object JvLabel1: TJvLabel
    Left = 395
    Top = 333
    Width = 187
    Height = 16
    Hint = 'jvcl\examples\Diagram1WebSiteScanner\DiagrammingClassArticle.doc'
    Caption = 'Diagramming with Delphi Part 1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = JvLabelClick
    OnMouseEnter = JvLabelMouseEnter
    OnMouseLeave = JvLabelMouseLeave
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvLabel2: TJvLabel
    Left = 395
    Top = 349
    Width = 187
    Height = 16
    Hint = 
      'jvcl\examples\Diagram2UseCaseEditor\Diagramming with Delphi Part' +
      ' 2.doc'
    Caption = 'Diagramming with Delphi Part 2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = JvLabelClick
    OnMouseEnter = JvLabelMouseEnter
    OnMouseLeave = JvLabelMouseLeave
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label13: TLabel
    Left = 20
    Top = 443
    Width = 323
    Height = 16
    Caption = 'There are several examples for JvPlugin comp'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object JvLabel3: TJvLabel
    Left = 34
    Top = 459
    Width = 186
    Height = 16
    Hint = 'jvcl\examples\JvPlugin'
    Caption = 'open JvPlugin examples folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = JvLabelClick
    OnMouseEnter = JvLabelMouseEnter
    OnMouseLeave = JvLabelMouseLeave
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label14: TLabel
    Left = 371
    Top = 56
    Width = 99
    Height = 16
    Caption = 'JCL Examples'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object JvLabel4: TJvLabel
    Left = 387
    Top = 73
    Width = 200
    Height = 16
    Hint = '\JCL\examples'
    Caption = 'open the sample folder of the JCL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = JvLabelClick
    OnMouseEnter = JvLabelMouseEnter
    OnMouseLeave = JvLabelMouseLeave
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label15: TLabel
    Left = 396
    Top = 93
    Width = 317
    Height = 52
    Caption = 
      'The included JEDI Code Library (JCL) has also many examples.'#13#10'Es' +
      'pecially examples for getting Source location infos. With'#13#10'this ' +
      'you can get in case of exceptions the unit name, the procedure'#13#10 +
      'name and even the line number! Very useful for error tracking.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 509
    Width = 737
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
