object frmSelectDateTimeDlg: TfrmSelectDateTimeDlg
  Left = 512
  Top = 361
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select date time'
  ClientHeight = 144
  ClientWidth = 254
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblDate: TLabel
    Left = 8
    Top = 8
    Width = 26
    Height = 13
    Caption = '&Date:'
  end
  object lblTime: TLabel
    Left = 8
    Top = 56
    Width = 26
    Height = 13
    Caption = '&Time:'
  end
  object dtpDate: TDateTimePicker
    Left = 8
    Top = 24
    Width = 238
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    CalAlignment = dtaLeft
    Date = 1
    Time = 1
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkDate
    ParseInput = False
    TabOrder = 0
  end
  object dtpTime: TDateTimePicker
    Left = 8
    Top = 72
    Width = 238
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    CalAlignment = dtaLeft
    Date = 1
    Time = 1
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkTime
    ParseInput = False
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 27
    Top = 107
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 147
    Top = 107
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object PopupMenu1: TPopupMenu
    Left = 120
    Top = 40
    object mnuNow: TMenuItem
      Caption = 'Now'
      OnClick = mnuNowClick
    end
    object mnuDate: TMenuItem
      Caption = 'Date'
      OnClick = mnuDateClick
    end
    object mnuTime: TMenuItem
      Caption = 'Time'
      OnClick = mnuTimeClick
    end
  end
end
