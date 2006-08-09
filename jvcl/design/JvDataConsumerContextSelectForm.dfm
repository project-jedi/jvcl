inherited frmDataConsumerContextSelect: TfrmDataConsumerContextSelect
  Caption = 'Select context...'
  ClientWidth = 600
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 235
    Width = 600
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TButton
      Left = 440
      Top = 5
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 520
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  inline fmeTreeList: TfmeJvProviderTreeList
    Left = 0
    Top = 0
    Width = 600
    Height = 235
    Align = alClient
    AutoScroll = False
    TabOrder = 1
    inherited lvProvider: TListView
      Width = 600
      Height = 235
    end
  end
end
