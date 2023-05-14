# frozen_string_literal: true

require 'spec_helper'

describe `(s-upper-camel-case (string-replace "spec" "" (file-name-nondirectory (file-name-base (buffer-file-name)))))` do
  it 'should do something' do
    $0
  end
end
