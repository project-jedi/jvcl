inherited frmJvDataConsumerItemSelect: TfrmJvDataConsumerItemSelect
  Caption = 'frmJvDataConsumerItemSelect'
  ClientHeight = 343
  ClientWidth = 564
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 310
    Width = 564
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOK: TButton
      Left = 405
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 485
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  inline fmeTreeList: TfmeJvProviderTreeList
    Left = 0
    Top = 0
    Width = 564
    Height = 310
    Align = alClient
    AutoScroll = False
    TabOrder = 1
    inherited lvProvider: TListView
      Width = 564
      Height = 310
      OnDblClick = fmeTreeListlvProviderDblClick
    end
  end
end
