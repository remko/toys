/*
 * Copyright (c) 2013 Remko Tronçon
 * Licensed under the simplified BSD license.
 */

#include <gmock/gmock.h>
#include <boost/coroutine/coroutine.hpp>
#include <boost/smart_ptr/make_shared.hpp>
#include <boost/shared_ptr.hpp>
#include <functional>

using namespace testing;
using namespace std;
using namespace std::tr1::placeholders;
using boost::coroutines::coroutine;

// Not all platforms come with std::shared_ptr yet
using boost::make_shared;
using boost::shared_ptr; 


/**
 * Tree datatype
 */
template<typename T> struct Node {
  Node(T value, shared_ptr<Node<T>> left, shared_ptr<Node<T>> right) :
    value(value), left(left), right(right) {
  }

  T value;
  shared_ptr<Node<T>> left;
  shared_ptr<Node<T>> right;
};

template<typename T> shared_ptr<Node<T>> node(
    T value, 
    shared_ptr<Node<T>> left = shared_ptr<Node<T>>(), 
    shared_ptr<Node<T>> right = shared_ptr<Node<T>>()) {
  return make_shared<Node<T>>(value, left, right);
}

/**
 * Walk a tree in in-order, and call 'c' for each value.
 */
template<typename T>
void walk(shared_ptr<Node<T>> node, typename coroutine<int()>::caller_type& c) {
  if (node) {
    walk(node->left, c);
    c(node->value);
    walk(node->right, c);
  }
}


/**
 * Example trees
 */
auto tree1 =
  node(8,
      node(4, node(2), node(5)),
      node(12, node(10), node(16)));
auto tree2 =
  node(9, 
      node(5, node(3), node(6)),
      node(13, node(11), node(17)));

/**
 * Collect the merged values into a vector.
 */
TEST(MergeBST, CollectValues) {
  coroutine<int()> flatTree1(bind(walk<int>, tree1, _1));
  coroutine<int()> flatTree2(bind(walk<int>, tree2, _1));

  vector<int> mergedTrees;
  merge(
      boost::begin(flatTree1), boost::end(flatTree1), 
      boost::begin(flatTree2), boost::end(flatTree2), 
      back_inserter(mergedTrees));

  const int expectedElements[] = { 2, 3, 4, 5, 5, 6, 8, 9, 10, 11, 12, 13, 16, 17 };
  ASSERT_THAT(mergedTrees, ElementsAreArray(expectedElements));
}

/**
 * Print the merged values directly to stdout.
 */
TEST(MergeBST, PrintValues) {
  coroutine<int()> flatTree1(bind(walk<int>, tree1, _1));
  coroutine<int()> flatTree2(bind(walk<int>, tree2, _1));
  merge(
      boost::begin(flatTree1), boost::end(flatTree1), 
      boost::begin(flatTree2), boost::end(flatTree2), 
      ostream_iterator<int>(cout, " "));
}

