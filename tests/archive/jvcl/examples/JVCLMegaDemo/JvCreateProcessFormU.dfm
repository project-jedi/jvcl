object JvCreateProcessForm: TJvCreateProcessForm
  Left = 298
  Top = 123
  Width = 569
  Height = 440
  Caption = 'TJvCreateProcess example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 8
    Top = 16
    Width = 537
    Height = 377
    Buttons = []
    Caption = 'a JvCreateProcess example using notepad'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'MS Shell Dlg 2'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
    object Panel1: TPanel
      Left = 24
      Top = 8
      Width = 497
      Height = 41
      TabOrder = 0
      object RunBtn: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = '&Run'
        TabOrder = 0
        OnClick = RunBtnClick
      end
      object QuitBtn: TButton
        Left = 288
        Top = 8
        Width = 97
        Height = 25
        Caption = 'Send WM_QUIT'
        TabOrder = 3
        OnClick = QuitBtnClick
      end
      object CloseBtn: TButton
        Left = 184
        Top = 8
        Width = 97
        Height = 25
        Caption = 'Send WM_CLOSE'
        TabOrder = 2
        OnClick = CloseBtnClick
      end
      object TerminateBtn: TButton
        Left = 392
        Top = 8
        Width = 97
        Height = 25
        Caption = 'Terminate process'
        TabOrder = 4
        OnClick = TerminateBtnClick
      end
      object StopBtn: TButton
        Left = 88
        Top = 8
        Width = 75
        Height = 25
        Caption = '&Stop waiting'
        TabOrder = 1
        OnClick = StopBtnClick
      end
    end
    object RichEdit1: TRichEdit
      Left = 24
      Top = 49
      Width = 497
      Height = 309
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      HideScrollBars = False
      ParentFont = False
      PlainText = True
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
  end
  object JvCreateProcess1: TJvCreateProcess
    CommandLine = 'notepad.exe'
    OnTerminate = JvCreateProcess1Terminate
    Left = 16
    Top = 56
  end
end
