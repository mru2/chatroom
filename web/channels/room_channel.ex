defmodule Chatroom.RoomChannel do
  use Phoenix.Channel

  def join("room", _message, socket) do
    send self(), :after_join
    {:ok, socket}
  end

  def handle_info(:after_join, socket) do
    push socket, "set_messages", %{
      messages: [
        %{author: "mru2", content: "Hello"},
        %{author: "MrRuru", content: "Hey there!"}
      ]
    }
    broadcast! socket, "new_message", %{"message" => %{
      author: "-",
      content: "Someone joined the room"
    }}
    {:noreply, socket}
  end

  def handle_in("message", payload = %{"author" => _author, "content" => _content}, socket) do
    broadcast! socket, "new_message", %{"message" => payload}
    {:noreply, socket}
  end

end
