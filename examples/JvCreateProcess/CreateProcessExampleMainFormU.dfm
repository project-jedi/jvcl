object CreateProcessExampleMainForm: TCreateProcessExampleMainForm
  Left = 382
  Top = 134
  Width = 544
  Height = 375
  Caption = 'TJvCreateProcess example (with notepad.exe)'
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 505
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 41
    Align = alTop
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
    Left = 0
    Top = 41
    Width = 536
    Height = 304
    Align = alClient
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
  object JvCreateProcess1: TJvCreateProcess
    CommandLine = 'notepad.exe'
    OnTerminate = JvCreateProcess1Terminate
    Left = 16
    Top = 56
  end
end
