unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  slack;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSimple: TButton;
    btnAdvanced: TButton;
    edWebHookURL: TEdit;
    edChannel: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure btnAdvancedClick(Sender: TObject);
    procedure btnSimpleClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnSimpleClick(Sender: TObject);
var
  MySlack: TSlackMessage;
begin
  MySlack := TSlackMessage.Create(edWebHookURL.Text);
  MySlack.Channel := edChannel.Text;
  MySlack.Message := Memo1.Text;
  MySlack.SendMessage;
  FreeAndNil(MySlack);
end;

procedure TForm1.btnAdvancedClick(Sender: TObject);
var
  MySlack: TSlackMessage;
  MyAttachment1: TSlackAttachment;
  MyAttachment2: TSlackAttachment;
  MyField1: TAttachmentField;
  MyField2: TAttachmentField;
  MyField3: TAttachmentField;
  MyField4: TAttachmentField;
begin
  // create Slack message
  MySlack := TSlackMessage.Create(edWebHookURL.Text);
  MySlack.Channel := edChannel.Text;

  MySlack.Message := 'Main Message';

  // create Attachment -1
  MyAttachment1 := TSlackAttachment.Create;
  MyAttachment1.FallBack := Memo1.Text;
  MyAttachment1.AuthorName := 'Lazarus';
  MyAttachment1.Color := '#FF6633';

  // create field-1 for Attachment
  MyField1 := TAttachmentField.Create;
  MyField1.Title := 'BIG Title - 1';
  MyField1.Value := Memo1.Text;
  MyField1.Short := True;

  MyAttachment1.AddField(MyField1);

  // create field-2 for Attachment
  MyField2 := TAttachmentField.Create;
  MyField2.Title := 'BIG Title - 2';
  MyField2.Value := Memo1.Text;
  MyField2.Short := True;

  // add field to Attachment
  MyAttachment1.AddField(MyField2);

  // Add Attachment to Slack Message
  MySlack.AddAttachment(MyAttachment1);

  // create Attachment-2
  MyAttachment2 := TSlackAttachment.Create;
  MyAttachment2.FallBack := Memo1.Text;
  MyAttachment2.AuthorName := 'Free Pascal';
  MyAttachment2.Color := '#3366FF';

  // create field-1 for Attachment
  MyField3 := TAttachmentField.Create;
  MyField3.Title := 'BIG Title - 3';
  MyField3.Value := Memo1.Text;
  MyField3.Short := False;

  MyAttachment2.AddField(MyField3);

  // create field-2 for Attachment
  MyField4 := TAttachmentField.Create;
  MyField4.Title := 'BIG Title - 4';
  MyField4.Value := Memo1.Text;
  MyField4.Short := False;

  // add field to Attachment
  MyAttachment2.AddField(MyField4);

  // Add Attachment to Slack Message
  MySlack.AddAttachment(MyAttachment2);



  // Send Message
  MySlack.SendMessage;

  FreeAndNil(MyField1);
  FreeAndNil(MyField2);
  FreeAndNil(MyAttachment1);
  FreeAndNil(MyField3);
  FreeAndNil(MyField4);
  FreeAndNil(MyAttachment2);
  FreeAndNil(MySlack);
end;

end.

