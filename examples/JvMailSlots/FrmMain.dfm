object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'JvMailSlots Demo'
  ClientHeight = 115
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grpClient: TGroupBox
    Left = 8
    Top = 8
    Width = 233
    Height = 65
    Caption = ' MailSlot Client '
    TabOrder = 0
    object edtClientText: TEdit
      Left = 16
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Hallo World!'
    end
    object btnClientSendText: TButton
      Left = 143
      Top = 26
      Width = 75
      Height = 25
      Caption = 'Send &Text'
      TabOrder = 1
      OnClick = btnClientSendTextClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 78
    Width = 486
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      486
      37)
    object bvlSplitter: TBevel
      Left = 0
      Top = 0
      Width = 486
      Height = 3
      Align = alTop
      Shape = bsTopLine
    end
    object lblInfo: TLabel
      Left = 8
      Top = 12
      Width = 189
      Height = 13
      Caption = 'Sending data can take some time'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnQuit: TButton
      Left = 404
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Quit'
      TabOrder = 0
      OnClick = btnQuitClick
    end
  end
  object grpServer: TGroupBox
    Left = 247
    Top = 8
    Width = 234
    Height = 65
    Caption = ' MailSlot Server '
    TabOrder = 2
    object edtServerText: TEdit
      Left = 16
      Top = 28
      Width = 207
      Height = 21
      TabOrder = 0
    end
  end
  object msServer: TJvMailSlotServer
    MailSlotName = 'MailSlotDemo'
    OnNewMessage = msServerNewMessage
    Left = 448
    Top = 8
  end
  object msClient: TJvMailSlotClient
    MailSlotName = 'MailSlotDemo'
    Left = 200
  end
end
