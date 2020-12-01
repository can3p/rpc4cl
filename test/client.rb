#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require "test/unit"
require "base64"
require "xmlrpc/client"

class XMLRPCValidation < Test::Unit::TestCase
  
  attr_reader :api
  
  def setup
    @api = XMLRPC::Client.new2("http://localhost:8008/validation")
  end
  
  def test_arrayOfStructsTest
    structs = [ {"larry" => 10, "curly" => 1, "moe" => 5},
                {"larry" => 10, "curly" => 8, "moe" => 5},
                {"larry" => 10, "curly" => 5, "moe" => 5} ]
    
    curlySum = self.api.call("validator1.arrayOfStructsTest", structs)
    assert( curlySum == 14, "Should be fourteen")
  end

  def test_countTheEntities
    entityCount = self.api.call("validator1.countTheEntities", "<<<&&>'''\"'\"'>>")
    assert( entityCount["ctLeftAngleBrackets"] == 3, "Number of left angle brackets" )
    assert( entityCount["ctRightAngleBrackets"] == 3, "Number of right angle brackets" )
    assert( entityCount["ctAmpersands"] == 2, "Number of ampersands" )
    assert( entityCount["ctApostrophes"] == 5, "Number of apostrophes" )
    assert( entityCount["ctQuotes"] == 2, "Number of quotes" )
  end

  def test_easyStructTest 
    sum = self.api.call("validator1.easyStructTest", {"moe" => 1, "curly" => 2, "larry" => 3})
    assert( sum == 6, "Sum should be six" )
  end

  def test_echoStructTest
    struct = { "a" => 1, "b" => 2 }
    assert(self.api.call("validator1.echoStructTest", struct) == struct, "Structs are equal")
  end

  def test_manyTypesTest
    april1st = XMLRPC::Convert.dateTime("2009-04-01T00:00:00")
    list = self.api.call("validator1.manyTypesTest", 4, true, "Hello Värld", 8.32, april1st, Base64.encode64("ÅÄÖ"))
    assert(list[0] == 4, "Should be four")
    assert(list[1] == true, "Should be true")
    assert(list[2] == "Hello Värld", "Should be the string we sent")
    assert(list[3] == 8.32, "Should be the double we sent")
    assert(list[4].year == 2009, "The year of the date we sent")
    assert(list[4].month == 4, "The month of the date we sent")
    assert(list[4].day == 1, "The day of the date we sent")
    assert(list[4].hour == 0, "The hour of the date we sent")
    assert(list[4].min == 0, "The minute of the date we sent")
    assert(list[4].sec == 0, "The second of the date we sent")
    assert(Base64.decode64(list[5]) == "ÅÄÖ", "The base64-encoded string we sent")
  end

  def test_moderateSizeArrayCheck
    words = ["Hello "]
    150.times { words.push("irrelevant") }
    words.push("World")
    greeting = self.api.call("validator1.moderateSizeArrayCheck", words)
    assert(greeting == "Hello World", "Greeting should be Hello World")
  end

  def test_nestedStructTest
    calendar = { "2000" => { "04" => { "01" => { "curly" => 1, "moe" => 4, "larry" => 8}}}}
    sum = self.api.call("validator1.nestedStructTest", calendar)
    assert(sum == 13, "Sum should be thirteen")
  end

  def test_simpleStructReturnTest
    m1 = self.api.call("validator1.simpleStructReturnTest", 12.5)
    m2 = self.api.call("validator1.simpleStructReturnTest", 12)

    assert(m1["times10"] == 125.0, "Should be a double of 125.0")
    assert(m1["times100"] == 1250.0, "Should be a double of 1250.0")
    assert(m1["times1000"] == 12500.0, "Should be a double of 12500.0")

    assert(m2["times10"] == 120, "Should be a int of 125")
    assert(m2["times100"] == 1200, "Should be a int of 1250")
    assert(m2["times1000"] == 12000, "Should be a int of 12500")
  end

  def test_multicall
    multiresult = self.api.multicall(["validator1.echoStructTest", { "a" => 1, "b" => 2 }],
                                     ["validator1.easyStructTest", {"moe" => 1, "curly" => 2, "larry" => 3}],
                                     ["validator1.echoStructTest", { "c" => 3, "d" => 4 }])

    assert(multiresult == [{ "a" => 1, "b" => 2 }, 6, { "c" => 3, "d" => 4 }], "Structs are equal")
  end

  def test_timeout
    exception = assert_raise XMLRPC::FaultException do
      self.api.call("validator1.timeoutTest")
    end
    assert(exception.faultCode() == 0)
  end

  def test_intInsteadOfStruct
    exception = assert_raise XMLRPC::FaultException do
      self.api.call("validator1.easyStructTest", 1)
    end
  end

  def test_emptyStringLength
    slen = self.api.call("validator1.stringLength", "")
    assert(slen == 0, "Length of empty string is 0")
  end

  def test_methodHelp
    doc = self.api.call("system.methodHelp", "validator1.stringLength")
    assert(doc == "Return the length of the string argument", "methodHelp is returned correctly")
  end

  def test_systemListMethods
    methodnames = self.api.call("system.listMethods")
    assert(methodnames.length > 10, "There are alot of methods")
  end

  def test_systemGetCapabilities
    result = self.api.call("system.getCapabilities")
    assert(result["introspect"]["specUrl"] == "http://xmlrpc-c.sourceforge.net/xmlrpc-c/introspection.html", "Supports introspection")
    assert(result["introspect"]["specVersion"] == 1, "Support introspection version 1")
  end

  def test_systemListMethods
    assert(self.api.call("system.methodSignature", "system.listMethods") == "undef", "Doesn't support methodSignature")
  end

  def test_performance
    # Testing with rpc4cl as client I get round 250 calls/second but
    # ruby is not as swift so this is just a basic sanity check
    start = Time.now.to_i
    10.times.each do
      Thread.new { 5.times.each { self.api.call("system.listMethods") } }.join
    end
    stop = Time.now.to_i
    assert(stop-start < 5, "Should perform 50 calls within 5 seconds")
  end

end
