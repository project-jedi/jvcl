object FormMake: TFormMake
  Left = 278
  Top = 222
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Installing Packages'
  ClientHeight = 161
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LblOpenFile: TLabel
    Left = 8
    Top = 136
    Width = 409
    Height = 13
    Cursor = crHandPoint
    AutoSize = False
    Caption = 'LblOpenFile'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Visible = False
    OnClick = LblOpenFileClick
  end
  object BtnAbort: TButton
    Left = 430
    Top = 131
    Width = 75
    Height = 25
    Caption = '&Abort'
    TabOrder = 0
    OnClick = BtnAbortClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 497
    Height = 116
    ActivePage = TabSheetProgress
    TabOrder = 1
    object TabSheetProgress: TTabSheet
      Caption = 'Progress'
      OnShow = TabSheetProgressShow
      object LblAction: TLabel
        Left = 8
        Top = 40
        Width = 44
        Height = 13
        Caption = 'LblAction'
        Transparent = True
      end
      object LblTarget: TLabel
        Left = 8
        Top = 8
        Width = 55
        Height = 13
        Caption = 'LblTarget'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object ProgressBar: TProgressBar
        Left = 8
        Top = 56
        Width = 473
        Height = 17
        Min = 0
        Max = 100
        TabOrder = 1
      end
      object ProgressBarTargets: TProgressBar
        Left = 8
        Top = 24
        Width = 473
        Height = 9
        Min = 0
        Max = 100
        Smooth = True
        TabOrder = 0
      end
    end
    object TabSheetLog: TTabSheet
      Caption = 'Compiler output'
      ImageIndex = 1
      OnShow = TabSheetLogShow
      object MemoLog: TRichEdit
        Left = 0
        Top = 0
        Width = 489
        Height = 88
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnKeyDown = MemoLogKeyDown
        OnSelectionChange = MemoLogSelectionChange
      end
    end
  end
end
