unit LinkedLists;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  DBRecord;

// TODO: generic?
type
  PNode = ^TNode;
  TNode = record
    data: TRecord;
    next: PNode;
  end;

procedure append(var head: PNode; var data: TRecord);
procedure disposelist(head: PNode);

implementation

procedure append(var head: PNode; var data: TRecord);
var
  NewNode, tail: PNode;
begin
  new(NewNode);
  NewNode^.next := nil;
  NewNode^.data := data;

  if head = nil then
  begin
    head := NewNode;
    exit;
  end;

  // Найти хвост
  tail := head;
  while tail^.next <> nil do
    tail := tail^.next;

  tail^.next := NewNode;
end;

procedure disposelist(head: PNode);
var
  next: PNode;
begin
  while head <> nil do
  begin
    next := head^.next;
    dispose(head);
    head := next;
  end;
end;

end.

