object frmMain: TfrmMain
  Left = 548
  Top = 124
  Width = 410
  Height = 454
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'JvTrayIcon Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 385
    Height = 361
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Tray Icon Options '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 56
      Width = 23
      Height = 13
      Caption = 'Hint:'
    end
    object chkActive: TCheckBox
      Left = 16
      Top = 32
      Width = 170
      Height = 17
      Caption = 'Active'
      TabOrder = 0
      OnClick = chkActiveClick
    end
    object edHint: TEdit
      Left = 16
      Top = 72
      Width = 353
      Height = 21
      TabOrder = 1
      Text = 'JvTrayIcon Demo'
    end
    object chkSnap: TCheckBox
      Left = 16
      Top = 112
      Width = 170
      Height = 17
      Caption = 'Snap'
      TabOrder = 2
    end
    object chkTaskBar: TCheckBox
      Left = 16
      Top = 136
      Width = 170
      Height = 17
      Caption = 'Show in taskbar'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object chkTaskList: TCheckBox
      Left = 16
      Top = 160
      Width = 170
      Height = 17
      Caption = 'Show in task list'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object chkAutoHide: TCheckBox
      Left = 16
      Top = 184
      Width = 170
      Height = 17
      Caption = 'Auto hide'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object chkRestoreClick: TCheckBox
      Left = 16
      Top = 208
      Width = 170
      Height = 17
      Caption = 'Restore on click'
      TabOrder = 6
      OnClick = chkRestoreClickClick
    end
    object chkRestoreDblClick: TCheckBox
      Left = 16
      Top = 232
      Width = 170
      Height = 17
      Caption = 'Restore on double-click'
      TabOrder = 7
      OnClick = chkRestoreDblClickClick
    end
    object chkMinClick: TCheckBox
      Left = 16
      Top = 256
      Width = 170
      Height = 17
      Caption = 'Minimize on click'
      TabOrder = 8
      OnClick = chkMinClickClick
    end
    object chkMinDblClick: TCheckBox
      Left = 16
      Top = 280
      Width = 170
      Height = 17
      Caption = 'Minimize on double-click'
      TabOrder = 9
      OnClick = chkMinDblClickClick
    end
    object chkPopUp: TCheckBox
      Left = 16
      Top = 304
      Width = 170
      Height = 17
      Caption = 'Menu on right click'
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
    object btnUpdate: TButton
      Left = 288
      Top = 320
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Update'
      TabOrder = 12
      OnClick = btnUpdateClick
    end
    object chkDropDown: TCheckBox
      Left = 16
      Top = 328
      Width = 170
      Height = 17
      Caption = 'Menu on left click'
      TabOrder = 11
    end
    object GroupBox2: TGroupBox
      Left = 152
      Top = 112
      Width = 217
      Height = 193
      Caption = ' Balloon Hint: '
      TabOrder = 13
      object Label2: TLabel
        Left = 8
        Top = 24
        Width = 24
        Height = 13
        Caption = 'Title:'
      end
      object Label3: TLabel
        Left = 8
        Top = 64
        Width = 26
        Height = 13
        Caption = 'Text:'
      end
      object Label4: TLabel
        Left = 8
        Top = 112
        Width = 25
        Height = 13
        Caption = 'Icon:'
      end
      object edBalloonTitle: TEdit
        Left = 8
        Top = 40
        Width = 201
        Height = 21
        TabOrder = 0
        Text = 'Sample Title'
      end
      object edBalloonText: TEdit
        Left = 8
        Top = 80
        Width = 201
        Height = 21
        TabOrder = 1
        Text = 'Sample Text'
      end
      object btnBalloon: TButton
        Left = 128
        Top = 160
        Width = 75
        Height = 25
        Caption = 'Sho&w'
        Enabled = False
        TabOrder = 2
        OnClick = btnBalloonClick
      end
      object cbBalloonType: TComboBox
        Left = 8
        Top = 128
        Width = 201
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        Items.Strings = (
          '(none)'
          'Error'
          'Info'
          'Warning')
      end
    end
  end
  object chkAutoRestore: TCheckBox
    Left = 18
    Top = 390
    Width = 283
    Height = 17
    Caption = 'Do NOT restore automatically after 15 seconds'
    TabOrder = 1
    OnClick = chkAutoRestoreClick
  end
  object JvTrayIcon1: TJvTrayIcon
    Icon.Data = {
      0000010001002020040000000000E80200001600000028000000200000004000
      0000010004000000000000020000000000000000000000000000000000000000
      0000000080000080000000808000800000008000800080800000C0C0C0008080
      80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
      000000000000000000000000000000000007FFFFFFFFFF778800000000000000
      8FFF77777777777777880000000000008FFFFF77777777777788000000000000
      088777F77777777778800000000000000008888F777777888800000000000000
      0000008F77777800000000000000000000000000000000000000000000000888
      888888888888888888888880000008F7777777777777777777777788000008F7
      FFFFFFFFFFFFFFFFFFFFF788800008F744444444444444444444F788800008F7
      0CCCCCCCCCCCCCCCCCC4F788800008F70CCCCCCCCCCCCCCCCCC4F788800008F7
      0CCCCCCCCCCCCCCCCCC4F788800008F70CCCCCCCCCCCCCCCCCC4F788800008F7
      0CCCCCCCCCCCCCCCCCC4F788800008F70CCCCCCCCCCCCCCCCCC4F788800008F7
      0CCCCCCCCCCCCCCCCCC4F788800008F70CCCCCCCCCCCCCCCCCC4F788800008F7
      0CCCCCCCCCCCCCCCCCC4F788800008F70CCECCCCCCCCCCCCCCC4F788800008F7
      0CCECCCCCCCCCCCCCCC4F788800008F70CCECCCCCCCCCCCCCCC4F788800008F7
      0CCCCCCCCCCCCCCCCCC4F788800008F700000000000000000004F788800008F7
      88888888888888888888F788800008F77777777777777777777777888000008F
      FFFFFFFFFFFFFFFFFFFFFF88800000087777777777777777777777F880000000
      87777777777777777777777F800000000888888888888888888888888000FE00
      03FFF80000FFF000007FF000007FF80000FFFE0003FFFFC01FFFC000001F8000
      000F800000078000000380000003800000038000000380000003800000038000
      0003800000038000000380000003800000038000000380000003800000038000
      0003800000038000000380000003C0000003E0000003F0000003F8000007}
    IconIndex = -1
    PopupMenu = popTrayIcon
    Left = 264
    Top = 32
  end
  object popTrayIcon: TPopupMenu
    Left = 320
    Top = 32
    object mnuShowHide: TMenuItem
      Caption = 'Show / Hide'
      OnClick = mnuShowHideClick
    end
  end
  object RestoreTimer: TTimer
    Interval = 15000
    OnTimer = RestoreTimerTimer
    Left = 174
    Top = 156
  end
end
