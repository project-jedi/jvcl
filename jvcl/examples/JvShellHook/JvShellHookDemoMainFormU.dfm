object JvShellHookDemoMainForm: TJvShellHookDemoMainForm
  Left = 333
  Top = 148
  Width = 540
  Height = 280
  Hint = 
    'Try opening and closing programs, move focus between application' +
    's etc.'#13#10#13#10'If your keyboard has extra buttons for opening mail pr' +
    'ograms, '#13#10'raising/lowering the volume etc, try pressing those to' +
    ' see the messages'#13#10'being intercepted.'#13#10#13#10'If the Active checkbox ' +
    'is grayed out, this means your system doesn'#39't support '#13#10'the Regi' +
    'sterShellHookWindow/DeregisterShellHookWindow functions.'#13#10
  Caption = 'JvShellHook Demo'
  Color = clBtnFace
  Constraints.MinHeight = 280
  Constraints.MinWidth = 540
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = '&Messages:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnClear: TButton
    Left = 448
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    TabOrder = 3
    OnClick = btnClearClick
  end
  object chkActive: TCheckBox
    Left = 16
    Top = 197
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Active'
    TabOrder = 1
    OnClick = chkActiveClick
  end
  object chkNoRedraw: TCheckBox
    Left = 16
    Top = 220
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Don'#39't show &redraws'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object lvMessages: TListView
    Left = 8
    Top = 32
    Width = 515
    Height = 155
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Message'
        Width = 200
      end
      item
        Caption = 'wParam'
        Width = 100
      end
      item
        Caption = 'lParam'
        Width = 100
      end
      item
        AutoSize = True
        Caption = 'Result'
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnResize = lvMessagesResize
  end
end
